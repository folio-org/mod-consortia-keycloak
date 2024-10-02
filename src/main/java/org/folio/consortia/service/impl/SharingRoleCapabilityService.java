package org.folio.consortia.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import feign.FeignException;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.ObjectUtils;
import org.folio.consortia.client.RoleCapabilitiesClient;
import org.folio.consortia.domain.dto.SharingRoleCapabilityDeleteResponse;
import org.folio.consortia.domain.dto.SharingRoleCapabilityRequest;
import org.folio.consortia.domain.dto.SharingRoleCapabilityResponse;
import org.folio.consortia.domain.dto.SourceValues;
import org.folio.consortia.domain.entity.SharingRoleEntity;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.SharingRoleRepository;
import org.folio.consortia.service.BaseSharingService;
import org.folio.consortia.service.ConsortiumService;
import org.folio.consortia.service.PublicationService;
import org.folio.consortia.service.TenantService;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.service.SystemUserScopedExecutionService;
import org.springframework.core.task.TaskExecutor;
import org.springframework.http.HttpMethod;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

@Service
@Log4j2
public class SharingRoleCapabilityService extends BaseSharingService<SharingRoleCapabilityRequest,
  SharingRoleCapabilityResponse, SharingRoleCapabilityDeleteResponse, SharingRoleEntity> {

  private static final String ROLE_ID = "roleId";

  private final RoleCapabilitiesClient roleCapabilitiesClient;
  private final SharingRoleRepository sharingRoleRepository;

  public SharingRoleCapabilityService(TenantService tenantService, ConsortiumService consortiumService,
                                      SystemUserScopedExecutionService systemUserScopedExecutionService,
                                      PublicationService publicationService,
                                      FolioExecutionContext folioExecutionContext, ObjectMapper parentObjectMapper,
                                      TaskExecutor asyncTaskExecutor, RoleCapabilitiesClient roleCapabilitiesClient,
                                      SharingRoleRepository sharingRoleRepository) {
    super(tenantService, consortiumService, systemUserScopedExecutionService, publicationService,
      folioExecutionContext, parentObjectMapper, asyncTaskExecutor);
    this.roleCapabilitiesClient = roleCapabilitiesClient;
    this.sharingRoleRepository = sharingRoleRepository;
  }

  @Override
  protected UUID getConfigId(SharingRoleCapabilityRequest request) {
    return request.getRoleId();
  }

  @Override
  protected Object getPayload(SharingRoleCapabilityRequest request) {
    return request.getPayload();
  }

  @Override
  protected String getPayloadId(ObjectNode payload) {
    return payload.get(ROLE_ID).asText();
  }

  @Override
  protected String getUrl(SharingRoleCapabilityRequest request, HttpMethod httpMethod) {
    String url = request.getUrl();
    if (httpMethod.equals(HttpMethod.PUT) || httpMethod.equals(HttpMethod.DELETE)) {
      url = url.replace("capabilities", getConfigId(request) + "/capabilities");
    }
    return url;
  }

  @Override
  protected void validateSharingConfigRequestOrThrow(UUID roleId,
                                                     SharingRoleCapabilityRequest request) {
    if (ObjectUtils.notEqual(getConfigId(request), roleId)) {
      throw new IllegalArgumentException("Mismatch id in path to roleId in request body");
    }
    if (Objects.isNull(getPayload(request))) {
      throw new IllegalArgumentException("Payload must not be null");
    }
    if (!sharingRoleRepository.existsByRoleId(roleId)) {
      throw new ResourceNotFoundException(ROLE_ID, String.valueOf(roleId));
    }
  }

  @Override
  protected void syncConfigWithTenant(SharingRoleCapabilityRequest request) {
    UUID roleId = request.getRoleId();
    String tenantId = folioExecutionContext.getTenantId();
    log.debug("syncConfigWithTenant:: Trying to syncing sharing role table with role capabilities in tenant '{}'",
      tenantId);

    if (sharingRoleRepository.existsByRoleIdAndTenantIdAndIsCapabilitiesSharedTrue(roleId, tenantId)) {
      log.info("syncConfigWithTenant:: Role '{}' and capabilities with tenant '{}' already exists in sharing role table, No need to sync",
        roleId, tenantId);
      return;
    }

    syncSharingRoleWithRoleCapabilitiesInTenant(roleId, tenantId);
  }

  private void syncSharingRoleWithRoleCapabilitiesInTenant(UUID roleId, String tenantId) {
    try {
      roleCapabilitiesClient.getRoleCapabilitiesByRoleId(roleId.toString());
      log.info("syncConfigWithTenant:: Role '{}' and capabilities found in tenant '{}', but not found in sharing role table, " +
        " creating new record in sharing table", roleId, tenantId);

      var sharingRoleEntity = getSharingRoleEntity(roleId, tenantId);
      sharingRoleEntity.setIsCapabilitiesShared(true);
      sharingRoleRepository.save(sharingRoleEntity);
    } catch (FeignException.NotFound e) {
      log.info("syncSharingRoleWithRoleCapabilitiesInTenant:: Role '{}' and capabilities not found in tenant '{}'" +
          " and sharing role table, No need to sync", roleId, tenantId);
    }  catch (Exception e) {
      log.error("syncConfig:: Error while fetching role capabilities", e);
    }
  }

  @Override
  protected Set<String> findTenantsForConfig(SharingRoleCapabilityRequest request) {
    return sharingRoleRepository.findTenantsByRoleIdAndIsCapabilitiesSharedTrue(request.getRoleId());
  }

  @Override
  protected void saveSharingConfig(List<SharingRoleEntity> sharingRoleEntityList) {
    sharingRoleEntityList.forEach(sharingRoleEntity -> sharingRoleEntity.setIsCapabilitiesShared(true));
    sharingRoleRepository.saveAll(sharingRoleEntityList);
  }

  @Override
  protected void deleteSharingConfig(SharingRoleCapabilityRequest request) {
    var sharingRoleEntityList = sharingRoleRepository.findByRoleId(request.getRoleId());
    sharingRoleEntityList.forEach(sharingRoleEntity -> sharingRoleEntity.setIsCapabilitiesShared(false));
    sharingRoleRepository.saveAll(sharingRoleEntityList);
  }

  @Override
  protected SharingRoleEntity createSharingConfigEntityFromRequest(SharingRoleCapabilityRequest request,
                                                                   String tenantId) {
    return getSharingRoleEntity(request.getRoleId(), tenantId);
  }

  private SharingRoleEntity getSharingRoleEntity(UUID roleId, String tenantId) {
    return sharingRoleRepository.findByRoleIdAndTenantId(roleId, tenantId);
  }

  @Override
  protected SharingRoleCapabilityResponse createSharingConfigResponse(UUID createRoleCapabilitiesPCId,
                                                                         UUID updateRoleCapabilitiesPCId) {
    return new SharingRoleCapabilityResponse()
      .createRoleCapabilitiesPCId(createRoleCapabilitiesPCId)
      .updateRoleCapabilitiesPCId(updateRoleCapabilitiesPCId);
  }

  @Override
  protected SharingRoleCapabilityDeleteResponse createSharingConfigResponse(UUID publishRequestId) {
    return new SharingRoleCapabilityDeleteResponse()
      .pcId(publishRequestId);
  }

  @Override
  protected ObjectNode updatePayload(SharingRoleCapabilityRequest request, String sourceValue) {
    var payload = objectMapper.convertValue(getPayload(request), ObjectNode.class);
    return payload.set(TYPE, new TextNode(sourceValue));
  }

  @Override
  protected String getSourceValue(SourceValues sourceValue) {
    return sourceValue.getRoleValue();
  }
}
