package org.folio.consortia.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import feign.FeignException;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.folio.consortia.client.PoliciesClient;
import org.folio.consortia.domain.dto.PublicationRequest;
import org.folio.consortia.domain.dto.SharingPolicyDeleteResponse;
import org.folio.consortia.domain.dto.SharingPolicyRequest;
import org.folio.consortia.domain.dto.SharingPolicyResponse;
import org.folio.consortia.domain.dto.SourceValues;
import org.folio.consortia.domain.entity.SharingPolicyEntity;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.SharingPolicyRepository;
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
public class SharingPolicyService extends
  BaseSharingService<SharingPolicyRequest, SharingPolicyResponse, SharingPolicyDeleteResponse, SharingPolicyEntity> {

  private final PoliciesClient policiesClient;
  private final SharingPolicyRepository sharingPolicyRepository;

  public SharingPolicyService(TenantService tenantService, ConsortiumService consortiumService,
                              SystemUserScopedExecutionService systemUserScopedExecutionService,
                              PublicationService publicationService, FolioExecutionContext folioExecutionContext,
                              ObjectMapper parentObjectMapper, TaskExecutor asyncTaskExecutor, PoliciesClient policiesClient,
                              SharingPolicyRepository sharingPolicyRepository) {
    super(tenantService, consortiumService, systemUserScopedExecutionService, publicationService,
      folioExecutionContext, parentObjectMapper, asyncTaskExecutor);
    this.policiesClient = policiesClient;
    this.sharingPolicyRepository = sharingPolicyRepository;
  }

  @Override
  protected UUID getConfigId(SharingPolicyRequest request) {
    return request.getPolicyId();
  }

  @Override
  protected Object getPayload(SharingPolicyRequest request) {
    return request.getPayload();
  }

  @Override
  protected String getPayloadId(ObjectNode payload) {
    return payload.get("id").asText();
  }

  @Override
  protected String getSourceValue(SourceValues sourceValue) {
    return sourceValue.getPolicyValue();
  }

  @Override
  protected void validateSharingConfigRequestOrThrow(UUID policyId, SharingPolicyRequest request) {
    if (ObjectUtils.notEqual(getConfigId(request), policyId)) {
      throw new IllegalArgumentException("Mismatch id in path to policyId in request body");
    }
    if (Objects.isNull(getPayload(request))) {
      throw new IllegalArgumentException("Payload must not be null");
    }
    if (!sharingPolicyRepository.existsByPolicyId(policyId)) {
      throw new ResourceNotFoundException("policyId", String.valueOf(policyId));
    }
  }

  /**
   * Policy has unique id, so payload should be same for all tenant
   */
  @Override
  protected boolean shouldCompactRequests() {
    return true;
  }

  @Override
  protected void syncConfigWithTenants(Set<String> sharedConfigTenants, SharingPolicyRequest request) {
    String tenantId = folioExecutionContext.getTenantId();
    UUID policyId = request.getPolicyId();
    log.debug("syncConfig:: Trying to syncing sharing policy table with policy table for policy '{}'", policyId);

    if (sharingPolicyRepository.existsByPolicyIdAndTenantId(policyId, tenantId)) {
      log.info("syncConfig:: Policy '{}' with tenant '{}' already exists in sharing role table, No need to sync",
        policyId, tenantId);
      return;
    }
    syncSharingPolicyWithPolicyInTenant(request, tenantId, policyId);
  }

  private void syncSharingPolicyWithPolicyInTenant(SharingPolicyRequest request, String tenantId, UUID policyId) {
    systemUserScopedExecutionService.executeSystemUserScoped(tenantId, () -> {
      try {
        policiesClient.getPolicyById(policyId);
        log.info("syncConfig:: Policy '{}' found in tenant '{}', but not found in sharing policy table, " +
          "creating new entry", policyId, tenantId);

        var sharingPolicyEntity = createSharingConfigEntity(request.getPolicyId(), tenantId);
        sharingPolicyRepository.save(sharingPolicyEntity);
      } catch (FeignException.NotFound e) {
        log.info("syncConfig:: Policy '{}' not found in tenant '{}' and sharing policy table, No need to sync",
          policyId, tenantId);
      } catch (Exception e) {
        log.error("syncConfig:: Error while fetching policies", e);
        throw new IllegalStateException("Error while fetching policies", e);
      }
      return null;
    });
  }

  @Override
  protected Set<String> findTenantsForConfig(SharingPolicyRequest request) {
    return sharingPolicyRepository.findTenantsByPolicyId(request.getPolicyId());
  }

  @Override
  protected void saveSharingConfig(List<SharingPolicyEntity> sharingPolicyEntityList) {
    sharingPolicyRepository.saveAll(sharingPolicyEntityList);
  }

  @Override
  protected void deleteSharingConfig(SharingPolicyRequest request) {
    sharingPolicyRepository.deleteByPolicyId(request.getPolicyId());
  }

  @Override
  protected PublicationRequest buildPublicationRequestForTenant(SharingPolicyRequest request, String tenantId, HttpMethod method) {
    String urlForRequest = getUrl(request, method);
    return new PublicationRequest()
      .method(method.toString())
      .url(urlForRequest)
      .payload(getPayload(request))
      .tenants(Set.of(tenantId));
  }

  private String getUrl(SharingPolicyRequest request, HttpMethod httpMethod) {
    String url = request.getUrl();
    if (httpMethod.equals(HttpMethod.PUT) || httpMethod.equals(HttpMethod.DELETE)) {
      url += "/" + getConfigId(request);
    }
    return url;
  }

  @Override
  protected SharingPolicyEntity createSharingConfigEntityFromRequest(SharingPolicyRequest request, String tenantId) {
    return createSharingConfigEntity(request.getPolicyId(), tenantId);
  }

  private SharingPolicyEntity createSharingConfigEntity(UUID policyId, String tenantId) {
    return SharingPolicyEntity.builder()
      .id(UUID.randomUUID())
      .policyId(policyId)
      .tenantId(tenantId)
      .build();
  }

  @Override
  protected SharingPolicyResponse createSharingConfigResponse(List<UUID> createPcIds, List<UUID> updatePcIds) {
    var response = new SharingPolicyResponse();
    if (CollectionUtils.isNotEmpty(createPcIds)) {
      response.setCreatePCId(createPcIds.get(0));
    }
    if (CollectionUtils.isNotEmpty(updatePcIds)) {
      response.setUpdatePCId(updatePcIds.get(0));
    }
    return response;
  }

  @Override
  protected SharingPolicyDeleteResponse createSharingConfigDeleteResponse(List<UUID> pcIds) {
    var response = new SharingPolicyDeleteResponse();
    if (CollectionUtils.isNotEmpty(pcIds)) {
      response.setPcId(pcIds.get(0));
    }
    return response;
  }

  @Override
  protected ObjectNode updateSourcePayload(Object payload, String sourceValue) {
    var payloadNode = objectMapper.convertValue(payload, ObjectNode.class);
    return payloadNode.set(SOURCE, new TextNode(sourceValue));
  }
}
