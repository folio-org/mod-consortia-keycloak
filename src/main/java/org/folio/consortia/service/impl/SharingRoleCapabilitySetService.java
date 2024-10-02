package org.folio.consortia.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.ObjectUtils;
import org.folio.consortia.domain.dto.SharingRoleCapabilitySetDeleteResponse;
import org.folio.consortia.domain.dto.SharingRoleCapabilitySetRequest;
import org.folio.consortia.domain.dto.SharingRoleCapabilitySetResponse;
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
public class SharingRoleCapabilitySetService extends BaseSharingService<SharingRoleCapabilitySetRequest,
  SharingRoleCapabilitySetResponse, SharingRoleCapabilitySetDeleteResponse, SharingRoleEntity> {

  private static final String ROLE_ID = "roleId";

  private final SharingRoleRepository sharingRoleRepository;

  public SharingRoleCapabilitySetService(TenantService tenantService, ConsortiumService consortiumService,
                                         SystemUserScopedExecutionService systemUserScopedExecutionService,
                                         PublicationService publicationService,
                                         FolioExecutionContext folioExecutionContext, ObjectMapper parentObjectMapper,
                                         TaskExecutor asyncTaskExecutor,
                                         SharingRoleRepository sharingRoleRepository) {
    super(tenantService, consortiumService, systemUserScopedExecutionService, publicationService,
      folioExecutionContext, parentObjectMapper, asyncTaskExecutor);
    this.sharingRoleRepository = sharingRoleRepository;
  }

  @Override
  protected UUID getConfigId(SharingRoleCapabilitySetRequest request) {
    return request.getRoleId();
  }

  @Override
  protected Object getPayload(SharingRoleCapabilitySetRequest request) {
    return request.getPayload();
  }

  @Override
  protected String getPayloadId(ObjectNode payload) {
    return payload.get(ROLE_ID).asText();
  }

  @Override
  protected String getUrl(SharingRoleCapabilitySetRequest request, HttpMethod httpMethod) {
    String url = request.getUrl();
    if (httpMethod.equals(HttpMethod.PUT) || httpMethod.equals(HttpMethod.DELETE)) {
      url = url.replace("capability-sets", getConfigId(request) + "/capability-sets");
    }
    return url;
  }

  @Override
  protected void validateSharingConfigRequestOrThrow(UUID roleId,
                                                     SharingRoleCapabilitySetRequest request) {
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
  protected Set<String> findTenantsForConfig(SharingRoleCapabilitySetRequest request) {
    return sharingRoleRepository.findTenantsByRoleIdAndIsCapabilitySetsSharedTrue(request.getRoleId());
  }

  @Override
  protected void saveSharingConfig(List<SharingRoleEntity> sharingRoleEntityList) {
    sharingRoleEntityList.forEach(sharingRoleEntity -> sharingRoleEntity.setIsCapabilitySetsShared(true));
    sharingRoleRepository.saveAll(sharingRoleEntityList);
  }

  @Override
  protected void deleteSharingConfig(UUID roleId) {
    var sharingRoleEntityList = sharingRoleRepository.findByRoleId(roleId);
    sharingRoleEntityList.forEach(sharingRoleEntity -> sharingRoleEntity.setIsCapabilitySetsShared(false));
    sharingRoleRepository.saveAll(sharingRoleEntityList);
  }

  @Override
  protected SharingRoleEntity createSharingConfigEntityFromRequest(SharingRoleCapabilitySetRequest request,
                                                                   String tenantId) {
    return sharingRoleRepository.findByRoleIdAndTenantId(request.getRoleId(), tenantId);
  }

  @Override
  protected SharingRoleCapabilitySetResponse createSharingConfigResponse(UUID createRoleCapabilitySetsPCId,
                                                                         UUID updateRoleCapabilitySetsPCId) {
    return new SharingRoleCapabilitySetResponse()
      .createRoleCapabilitySetsPCId(createRoleCapabilitySetsPCId)
      .updateRoleCapabilitySetsPCId(updateRoleCapabilitySetsPCId);
  }

  @Override
  protected SharingRoleCapabilitySetDeleteResponse createSharingConfigResponse(UUID publishRequestId) {
    return new SharingRoleCapabilitySetDeleteResponse()
      .pcId(publishRequestId);
  }

  @Override
  protected ObjectNode updatePayload(SharingRoleCapabilitySetRequest request, String sourceValue) {
    var payload = objectMapper.convertValue(getPayload(request), ObjectNode.class);
    return payload.set(TYPE, new TextNode(sourceValue));
  }

  @Override
  protected String getSourceValue(SourceValues sourceValue) {
    return sourceValue.getRoleValue();
  }
}
