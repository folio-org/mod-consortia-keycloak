package org.folio.consortia.service.impl;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;

import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.folio.consortia.client.CapabilitySetsClient;
import org.folio.consortia.domain.dto.CapabilitySets;
import org.folio.consortia.domain.dto.PublicationRequest;
import org.folio.consortia.domain.dto.SharingRoleCapabilitySetDeleteResponse;
import org.folio.consortia.domain.dto.SharingRoleCapabilitySetRequest;
import org.folio.consortia.domain.dto.SharingRoleCapabilitySetResponse;
import org.folio.consortia.domain.dto.Tenant;
import org.folio.consortia.domain.dto.TenantCollection;
import org.folio.consortia.domain.entity.SharingRoleCapabilitySetEntity;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.SharingRoleCapabilitySetRepository;
import org.folio.consortia.service.BaseSharingService;
import org.folio.consortia.service.ConsortiumService;
import org.folio.consortia.service.PublicationService;
import org.folio.consortia.service.TenantService;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.context.ExecutionContextBuilder;
import org.folio.spring.scope.FolioExecutionContextSetter;
import org.folio.spring.service.SystemUserScopedExecutionService;
import org.springframework.core.task.TaskExecutor;
import org.springframework.http.HttpMethod;
import org.springframework.stereotype.Service;

@Service
@Log4j2
public class SharingRoleCapabilitySetService extends BaseSharingService<SharingRoleCapabilitySetRequest, SharingRoleCapabilitySetResponse, SharingRoleCapabilitySetDeleteResponse, SharingRoleCapabilitySetEntity> {

  private static final String TYPE = "type";

  private final SharingRoleCapabilitySetRepository sharingRoleCapabilitySetRepository;
  private final ExecutionContextBuilder contextBuilder;
  private final CapabilitySetsClient capabilitySetsClient;

  public SharingRoleCapabilitySetService(TenantService tenantService, ConsortiumService consortiumService,
                                         SystemUserScopedExecutionService systemUserScopedExecutionService,
                                         PublicationService publicationService, FolioExecutionContext folioExecutionContext,
                                         ObjectMapper parentObjectMapper, TaskExecutor asyncTaskExecutor,
                                         SharingRoleCapabilitySetRepository sharingRoleCapabilitySetRepository, ExecutionContextBuilder contextBuilder, CapabilitySetsClient capabilitySetsClient) {
    super(tenantService, consortiumService, systemUserScopedExecutionService, publicationService,
      folioExecutionContext, parentObjectMapper, asyncTaskExecutor);
    this.sharingRoleCapabilitySetRepository = sharingRoleCapabilitySetRepository;
    this.contextBuilder = contextBuilder;
    this.capabilitySetsClient = capabilitySetsClient;
  }

  @Override
  protected UUID getConfigId(SharingRoleCapabilitySetRequest sharingRoleCapabilitySetRequest) {
    return sharingRoleCapabilitySetRequest.getRoleId();
  }

  @Override
  protected Object getPayload(SharingRoleCapabilitySetRequest sharingRoleCapabilitySetRequest) {
    return sharingRoleCapabilitySetRequest.getPayload();
  }

  @Override
  protected void validateSharingConfigRequestOrThrow(UUID roleId, SharingRoleCapabilitySetRequest sharingRoleCapabilitySetRequest) {
    if (ObjectUtils.notEqual(getConfigId(sharingRoleCapabilitySetRequest), roleId)) {
      throw new IllegalArgumentException("Mismatch id in path to roleId in request body");
    }
    if (Objects.isNull(getPayload(sharingRoleCapabilitySetRequest))) {
      throw new IllegalArgumentException("Payload must not be null");
    }
    if (!sharingRoleCapabilitySetRepository.existsByRoleId(roleId)) {
      throw new ResourceNotFoundException("roleId", String.valueOf(roleId));
    }
  }

  @Override
  protected Set<String> findTenantsByConfigId(UUID roleId) {
    return sharingRoleCapabilitySetRepository.findTenantsByRoleId(roleId);
  }

  @Override
  protected void saveSharingConfig(List<SharingRoleCapabilitySetEntity> sharingRoleCapabilitySetEntityList) {
    sharingRoleCapabilitySetRepository.saveAll(sharingRoleCapabilitySetEntityList);
  }

  @Override
  protected void deleteSharingConfig(UUID roleId) {
    sharingRoleCapabilitySetRepository.deleteByRoleId(roleId);
  }

  @Override
  protected PublicationRequest createPublicationRequest(SharingRoleCapabilitySetRequest sharingRoleCapabilitySetRequest, String httpMethod) {
    PublicationRequest publicationRequest = new PublicationRequest();
    publicationRequest.setMethod(httpMethod);
    String url = sharingRoleCapabilitySetRequest.getUrl();
    if (httpMethod.equals(HttpMethod.PUT.toString()) || httpMethod.equals(HttpMethod.DELETE.toString())) {
      url += "/" + getConfigId(sharingRoleCapabilitySetRequest);
    }
    publicationRequest.setUrl(url);
    publicationRequest.setPayload(getPayload(sharingRoleCapabilitySetRequest));
    publicationRequest.setTenants(new HashSet<>());
    return publicationRequest;
  }

  @Override
  protected SharingRoleCapabilitySetEntity createSharingConfigEntityFromRequest(SharingRoleCapabilitySetRequest sharingRoleCapabilitySetRequest, String tenantId) {
    SharingRoleCapabilitySetEntity sharingRoleCapabilitySetEntity = new SharingRoleCapabilitySetEntity();
    sharingRoleCapabilitySetEntity.setId(UUID.randomUUID());
    sharingRoleCapabilitySetEntity.setRoleId(sharingRoleCapabilitySetEntity.getRoleId());
    sharingRoleCapabilitySetEntity.setTenantId(tenantId);
    return sharingRoleCapabilitySetEntity;
  }

  @Override
  protected SharingRoleCapabilitySetResponse createSharingConfigResponse(UUID createRolesPcId, UUID updateRolesPcId) {
    return new SharingRoleCapabilitySetResponse()
      .createRoleCapabilitySetsPCId(createRolesPcId)
      .updateRoleCapabilitySetsPCId(updateRolesPcId);
  }

  @Override
  protected SharingRoleCapabilitySetDeleteResponse createSharingConfigResponse(UUID publishRequestId) {
    return new SharingRoleCapabilitySetDeleteResponse()
      .pcId(publishRequestId);
  }

  @Override
  protected ObjectNode updatePayload(SharingRoleCapabilitySetRequest sharingRoleCapabilitySetRequest, String sourceValue) {
    JsonNode payload = objectMapper.convertValue(getPayload(sharingRoleCapabilitySetRequest), JsonNode.class);
    return ((ObjectNode) payload).set(TYPE, new TextNode(sourceValue));
  }

  @Override
  protected List<SharingRoleCapabilitySetEntity> linkTenantsToPublicationPutPostRequestAndEntity(TenantCollection allTenants,
                                                                          SharingRoleCapabilitySetRequest sharingConfigRequest,
                                                                          Set<String> sharingConfigTenants,
                                                                          PublicationRequest publicationPutRequest,
                                                                          PublicationRequest publicationPostRequest) {
    List<SharingRoleCapabilitySetEntity> sharingConfigEntityList = new ArrayList<>();
    for (Tenant tenant : allTenants.getTenants()) {
      if (sharingConfigTenants.contains(tenant.getId())) {
        publicationPutRequest.getTenants().add(tenant.getId());
        log.info("linkTenantsToPublicationPutPostRequestAndEntity:: tenant={} added to publication update request for {}={}",
          tenant.getId(), sharingConfigRequest.getClass().getName(), getConfigId(sharingConfigRequest));
      } else {
        publicationPostRequest.getTenants().add(tenant.getId());
        log.info("linkTenantsToPublicationPutPostRequestAndEntity:: tenant={} added to publication create request for {}={}",
          tenant.getId(), sharingConfigRequest.getClass().getName(), getConfigId(sharingConfigRequest));
        sharingConfigEntityList.add(createSharingConfigEntityFromRequest(sharingConfigRequest, tenant.getId()));
      }
    }

    List<CapabilitySets> capabilitySets = new ArrayList<>();
    try (var ignored = new FolioExecutionContextSetter(contextBuilder.buildContext(folioExecutionContext.getTenantId()))) {
//      capabilitySetsClient.queryCapabilitySets()
    }
    return sharingConfigEntityList;
  }

  @Override
  protected UUID publishRequest(UUID consortiumId, PublicationRequest publicationRequest) {
    if (CollectionUtils.isNotEmpty(publicationRequest.getTenants())) {
//      return publicationService.publishRequest(consortiumId, publicationRequest).getId();
    }
    log.info("publishRequest:: Tenant list of publishing for http method: {} is empty", publicationRequest.getMethod());
    return null;
  }
}
