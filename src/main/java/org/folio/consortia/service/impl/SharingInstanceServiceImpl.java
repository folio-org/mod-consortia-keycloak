package org.folio.consortia.service.impl;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import jakarta.transaction.Transactional;
import java.util.Objects;
import java.util.UUID;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.ObjectUtils;
import org.folio.consortia.client.InventoryClient;
import org.folio.consortia.config.kafka.KafkaService;
import org.folio.consortia.domain.dto.SharingInstance;
import org.folio.consortia.domain.dto.SharingInstanceCollection;
import org.folio.consortia.domain.dto.SourceValues;
import org.folio.consortia.domain.dto.Status;
import org.folio.consortia.domain.entity.SharingInstanceEntity;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.SharingInstanceRepository;
import org.folio.consortia.repository.SharingInstanceRepository.Specifications;
import org.folio.consortia.service.ConsortiumService;
import org.folio.consortia.service.InventoryService;
import org.folio.consortia.service.SharingInstanceService;
import org.folio.consortia.service.TenantService;
import org.folio.consortia.utils.TenantContextUtils;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.data.OffsetRequest;
import org.folio.spring.scope.FolioExecutionContextSetter;
import org.springframework.core.convert.ConversionService;
import org.springframework.stereotype.Service;

@Service
@Log4j2
@RequiredArgsConstructor
public class SharingInstanceServiceImpl implements SharingInstanceService {
  private static final String GET_INSTANCE_EXCEPTION_MSG = "Failed to get inventory instance with reason: %s";
  private static final String POST_INSTANCE_EXCEPTION_MSG = "Failed to post inventory instance with reason: %s";
  private final SharingInstanceRepository sharingInstanceRepository;
  private final ConsortiumService consortiumService;
  private final TenantService tenantService;
  private final ConversionService converter;
  private final InventoryService inventoryService;
  private final FolioModuleMetadata folioModuleMetadata;
  private final FolioExecutionContext folioExecutionContext;
  private final ObjectMapper objectMapper;
  private final KafkaService kafkaService;

  @Override
  public SharingInstance getById(UUID consortiumId, UUID actionId) {
    log.debug("getById:: Trying to get sharingInstance by consortiumId: {} and action id: {}", consortiumId, actionId);
    SharingInstanceEntity sharingInstanceEntity = sharingInstanceRepository.findById(actionId).
      orElseThrow(() -> new ResourceNotFoundException("actionId", String.valueOf(actionId)));
    log.info("getById:: sharingInstance object with id: {} was successfully retrieved", sharingInstanceEntity.getId());
    return converter.convert(sharingInstanceEntity, SharingInstance.class);
  }

  @Override
  @SneakyThrows
  @Transactional
  public SharingInstance start(UUID consortiumId, SharingInstance sharingInstance) {
    log.debug("start:: Trying to start instance sharing with instanceIdentifier: {}, consortiumId: {}", sharingInstance.getInstanceIdentifier(), consortiumId);
    consortiumService.checkConsortiumExistsOrThrow(consortiumId);

    String centralTenantId = tenantService.getCentralTenantId();
    String sourceTenantId = sharingInstance.getSourceTenantId();
    String targetTenantId = sharingInstance.getTargetTenantId();
    checkTenantsExistAndContainCentralTenantOrThrow(sourceTenantId, targetTenantId);

    if (Objects.equals(centralTenantId, sourceTenantId)) {
      JsonNode inventoryInstance;

      try (var ignored = new FolioExecutionContextSetter(
        TenantContextUtils.prepareContextForTenant(sourceTenantId, folioModuleMetadata, folioExecutionContext))) {
        inventoryInstance = inventoryService.getById(sharingInstance.getInstanceIdentifier());
      } catch (Exception ex) {
        log.error("start:: error when getting instance by id: {}", sharingInstance.getInstanceIdentifier(), ex);
        return updateFieldsAndSaveInCaseOfException(sharingInstance, GET_INSTANCE_EXCEPTION_MSG, ex);
      }

      try (var ignored = new FolioExecutionContextSetter(
        TenantContextUtils.prepareContextForTenant(targetTenantId, folioModuleMetadata, folioExecutionContext))) {
        String source = switch (inventoryInstance.get("source").asText().toLowerCase()) {
          case "folio" -> SourceValues.CONSORTIUM_FOLIO_INSTANCE.getValue();
          case "marc" -> SourceValues.CONSORTIUM_MARC_INSTANCE.getValue();
          case "linked_data" -> SourceValues.CONSORTIUM_LINKED_DATA_INSTANCE.getValue();
          default -> throw new IllegalStateException("source is not recognized");
        };
        var updatedInventoryInstance = ((ObjectNode) inventoryInstance).set("source", new TextNode(source));
        inventoryService.saveInstance(updatedInventoryInstance);
      } catch (Exception ex) {
        log.error("start:: error when posting instance with id: {}", sharingInstance.getInstanceIdentifier(), ex);
        return updateFieldsAndSaveInCaseOfException(sharingInstance, POST_INSTANCE_EXCEPTION_MSG, ex);
      }

      sharingInstance.setStatus(Status.COMPLETE);
    } else {
      String data = objectMapper.writeValueAsString(sharingInstance);
      kafkaService.send(KafkaService.Topic.CONSORTIUM_INSTANCE_SHARING_INIT, String.valueOf(sharingInstance.getId()), data);

      sharingInstance.setStatus(Status.IN_PROGRESS);
    }

    SharingInstanceEntity savedSharingInstance = saveSharingInstance(sharingInstance);
    log.info("start:: sharingInstance with id: {}, instanceId: {}, sourceTenantId: {}, targetTenantId: {} has been saved with status: {}",
      savedSharingInstance.getId(), savedSharingInstance.getInstanceId(), sourceTenantId, targetTenantId, savedSharingInstance.getStatus());
    return converter.convert(savedSharingInstance, SharingInstance.class);
  }

  /**
   * This method save sharing instance record to database.
   * Before to save sharing instance to db, previous attempt will be checked.
   * If a matching previous attempt is found, the method updates it with the new attempt's error and status information.
   * Otherwise, new record will be created.
   *
   * @param sharingInstance sharingInstanceDto
   * @return saved sharing instance entity
   */
  private SharingInstanceEntity saveSharingInstance(SharingInstance sharingInstance) {
    var existingSharingInstanceOptional = sharingInstanceRepository.findByInstanceAndTenantIds(
      sharingInstance.getInstanceIdentifier(), sharingInstance.getSourceTenantId(), sharingInstance.getTargetTenantId());

    if (existingSharingInstanceOptional.isEmpty()) {
      log.info("saveSharingInstance:: There is no existing record, so creating new record");
      return sharingInstanceRepository.save(toEntity(sharingInstance));
    }

    log.info("saveSharingInstance:: Existed sharingInstance is being updated with new attempt with status={}", sharingInstance.getStatus());
    var existingSharingInstance = existingSharingInstanceOptional.get();
    existingSharingInstance.setError(sharingInstance.getError());
    existingSharingInstance.setStatus(sharingInstance.getStatus());
    return sharingInstanceRepository.save(existingSharingInstance);
  }

  @Override
  public SharingInstanceCollection getSharingInstances(UUID consortiumId, UUID instanceIdentifier, String sourceTenantId,
                                                       String targetTenantId, Status status, Integer offset, Integer limit) {
    log.debug("getSharingInstances:: parameters consortiumId: {}, instanceIdentifier: {}, sourceTenantId: {}, targetTenantId: {}, status: {}.",
      consortiumId, instanceIdentifier, sourceTenantId, targetTenantId, status);
    consortiumService.checkConsortiumExistsOrThrow(consortiumId);
    var specification = Specifications.constructSpecification(instanceIdentifier, sourceTenantId, targetTenantId, status);

    var sharingInstancePage = sharingInstanceRepository.findAll(specification, OffsetRequest.of(offset, limit));
    var result = new SharingInstanceCollection();
    result.setSharingInstances(sharingInstancePage.stream().map(o -> converter.convert(o, SharingInstance.class)).toList());
    result.setTotalRecords((int) sharingInstancePage.getTotalElements());
    log.info("getSharingInstances:: total number of matched sharingInstances: {}.", result.getTotalRecords());
    return result;
  }

  @Override
  @Transactional
  public void completePromotingLocalInstance(String eventPayload) {
    log.debug("completePromotingLocalInstance:: parameters eventPayload: {}", eventPayload);
    try {
      var promotingEvent = objectMapper.readValue(eventPayload, SharingInstance.class);

      String centralTenantId = tenantService.getCentralTenantId();
      String sourceTenantId = promotingEvent.getSourceTenantId();
      String targetTenantId = promotingEvent.getTargetTenantId();
      checkTenantsExistAndContainCentralTenantOrThrow(sourceTenantId, targetTenantId);

      if (ObjectUtils.notEqual(centralTenantId, targetTenantId)) {
        log.warn("completePromotingLocalInstance:: promotion failed as targetTenantId: {} does not equal to centralTenantId: {}", targetTenantId, centralTenantId);
        return;
      }

      var specification = Specifications.constructSpecification(promotingEvent.getInstanceIdentifier(), sourceTenantId, targetTenantId, null);
      var optionalSharingInstance = sharingInstanceRepository.findOne(specification);

      if (optionalSharingInstance.isEmpty()) {
        log.warn("completePromotingLocalInstance:: sharingInstance with instanceIdentifier: {}, sourceTenantId: {}, targetTenantId: {} does not exist",
          promotingEvent.getInstanceIdentifier(), sourceTenantId, targetTenantId);
        return;
      }

      var promotedSharingInstance = optionalSharingInstance.get();
      if (ObjectUtils.isNotEmpty(promotingEvent.getError())) {
        promotedSharingInstance.setStatus(Status.ERROR);
        promotedSharingInstance.setError(promotingEvent.getError());
      } else {
        promotedSharingInstance.setStatus(Status.COMPLETE);
      }

      sharingInstanceRepository.save(promotedSharingInstance);
      log.info("completePromotingLocalInstance:: status of sharingInstance with instanceIdentifier: {}, sourceTenantId: {}, targetTenantId: {} " +
        "has been updated to: {}", promotedSharingInstance.getInstanceId(), sourceTenantId, targetTenantId, promotedSharingInstance.getStatus());
    } catch (Exception e) {
      log.error("completePromotingLocalInstance:: exception occurred while promoting local sharing instance", e);
    }
  }

  private void checkTenantsExistAndContainCentralTenantOrThrow(String sourceTenantId, String targetTenantId) {
    // both tenants should exist in the consortium
    tenantService.checkTenantExistsOrThrow(sourceTenantId);
    tenantService.checkTenantExistsOrThrow(targetTenantId);

    // at least one of the tenants should be 'centralTenant'
    String centralTenantId = tenantService.getCentralTenantId();
    if (Objects.equals(centralTenantId, sourceTenantId) || Objects.equals(centralTenantId, targetTenantId)) {
      return;
    }
    throw new IllegalArgumentException("Both 'sourceTenantId' and 'targetTenantId' cannot be member tenants.");
  }

  private SharingInstance updateFieldsAndSaveInCaseOfException(SharingInstance sharingInstance, String message, Exception ex) {
    sharingInstance.setStatus(Status.ERROR);
    sharingInstance.setError(String.format(message, InventoryClient.getReason(ex)));
    var savedSharingInstance = saveSharingInstance(sharingInstance);
    return converter.convert(savedSharingInstance, SharingInstance.class);
  }

  private SharingInstanceEntity toEntity(SharingInstance dto) {
    SharingInstanceEntity entity = new SharingInstanceEntity();
    entity.setId(UUID.randomUUID());
    entity.setInstanceId(dto.getInstanceIdentifier());
    entity.setSourceTenantId(dto.getSourceTenantId());
    entity.setTargetTenantId(dto.getTargetTenantId());
    entity.setStatus(dto.getStatus());
    entity.setError(dto.getError());
    return entity;
  }
}
