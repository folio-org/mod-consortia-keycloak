package org.folio.consortia.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.ObjectUtils;
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
public class SharingPolicyService extends BaseSharingService<SharingPolicyRequest, SharingPolicyResponse, SharingPolicyDeleteResponse, SharingPolicyEntity> {

  private final SharingPolicyRepository sharingPolicyRepository;

  public SharingPolicyService(TenantService tenantService, ConsortiumService consortiumService,
                              SystemUserScopedExecutionService systemUserScopedExecutionService,
                              PublicationService publicationService, FolioExecutionContext folioExecutionContext,
                              ObjectMapper parentObjectMapper, TaskExecutor asyncTaskExecutor, SharingPolicyRepository sharingPolicyRepository) {
    super(tenantService, consortiumService, systemUserScopedExecutionService, publicationService,
      folioExecutionContext, parentObjectMapper, asyncTaskExecutor);
    this.sharingPolicyRepository = sharingPolicyRepository;
  }

  @Override
  protected UUID getConfigId(SharingPolicyRequest sharingPolicyRequest) {
    return sharingPolicyRequest.getPolicyId();
  }

  @Override
  protected Object getPayload(SharingPolicyRequest sharingPolicyRequest) {
    return sharingPolicyRequest.getPayload();
  }

  @Override
  protected String getPayloadId(ObjectNode payload) {
    return payload.get("id").asText();
  }

  @Override
  protected String getUrl(SharingPolicyRequest request, HttpMethod httpMethod) {
    String url = request.getUrl();
    if (httpMethod.equals(HttpMethod.PUT) || httpMethod.equals(HttpMethod.DELETE)) {
      url += "/" + getConfigId(request);
    }
    return url;
  }

  @Override
  protected void validateSharingConfigRequestOrThrow(UUID policyId, SharingPolicyRequest sharingPolicyRequest) {
    if (ObjectUtils.notEqual(getConfigId(sharingPolicyRequest), policyId)) {
      throw new IllegalArgumentException("Mismatch id in path to policyId in request body");
    }
    if (Objects.isNull(getPayload(sharingPolicyRequest))) {
      throw new IllegalArgumentException("Payload must not be null");
    }
    if (!sharingPolicyRepository.existsByPolicyId(policyId)) {
      throw new ResourceNotFoundException("policyId", String.valueOf(policyId));
    }
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
  protected void deleteSharingConfig(UUID policyId) {
    sharingPolicyRepository.deleteByPolicyId(policyId);
  }

  @Override
  protected SharingPolicyEntity createSharingConfigEntityFromRequest(SharingPolicyRequest sharingPolicyRequest, String tenantId) {
    SharingPolicyEntity sharingPolicyEntity = new SharingPolicyEntity();
    sharingPolicyEntity.setId(UUID.randomUUID());
    sharingPolicyEntity.setPolicyId(sharingPolicyRequest.getPolicyId());
    sharingPolicyEntity.setTenantId(tenantId);
    return sharingPolicyEntity;
  }

  @Override
  protected SharingPolicyResponse createSharingConfigResponse(UUID createSettingsPcId, UUID updateSettingsPcId) {
    return new SharingPolicyResponse()
      .createPoliciesPCId(createSettingsPcId)
      .updatePoliciesPCId(updateSettingsPcId);

  }

  @Override
  protected SharingPolicyDeleteResponse createSharingConfigResponse(UUID publishRequestId) {
    return new SharingPolicyDeleteResponse()
      .pcId(publishRequestId);
  }

  @Override
  protected ObjectNode updatePayload(SharingPolicyRequest sharingConfigRequest, String sourceValue) {
    var payload = objectMapper.convertValue(getPayload(sharingConfigRequest), ObjectNode.class);
    return payload.set(SOURCE, new TextNode(sourceValue));
  }

  @Override
  protected String getSourceValue(SourceValues sourceValue) {
    return sourceValue.getPolicyValue();
  }
}
