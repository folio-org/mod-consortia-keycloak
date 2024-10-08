package org.folio.consortia.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.ObjectUtils;
import org.folio.consortia.domain.dto.SharingRoleDeleteResponse;
import org.folio.consortia.domain.dto.SharingRoleRequest;
import org.folio.consortia.domain.dto.SharingRoleResponse;
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
public class SharingRoleService extends BaseSharingService<SharingRoleRequest, SharingRoleResponse, SharingRoleDeleteResponse, SharingRoleEntity> {

  private final SharingRoleRepository sharingRoleRepository;

  public SharingRoleService(TenantService tenantService, ConsortiumService consortiumService,
                            SystemUserScopedExecutionService systemUserScopedExecutionService,
                            PublicationService publicationService, FolioExecutionContext folioExecutionContext,
                            ObjectMapper parentObjectMapper, TaskExecutor asyncTaskExecutor, SharingRoleRepository sharingRoleRepository) {
    super(tenantService, consortiumService, systemUserScopedExecutionService, publicationService,
      folioExecutionContext, parentObjectMapper, asyncTaskExecutor);
    this.sharingRoleRepository = sharingRoleRepository;
  }

  @Override
  protected UUID getConfigId(SharingRoleRequest request) {
    return request.getRoleId();
  }

  @Override
  protected Object getPayload(SharingRoleRequest request) {
    return request.getPayload();
  }

  @Override
  protected String getPayloadId(ObjectNode payload) {
    return payload.get("id").asText();
  }

  @Override
  protected String getUrl(SharingRoleRequest request, HttpMethod httpMethod) {
    String url = request.getUrl();
    if (httpMethod.equals(HttpMethod.PUT) || httpMethod.equals(HttpMethod.DELETE)) {
      url += "/" + getConfigId(request);
    }
    return url;
  }

  @Override
  protected void validateSharingConfigRequestOrThrow(UUID roleId, SharingRoleRequest request) {
    if (ObjectUtils.notEqual(getConfigId(request), roleId)) {
      throw new IllegalArgumentException("Mismatch id in path to roleId in request body");
    }
    if (Objects.isNull(getPayload(request))) {
      throw new IllegalArgumentException("Payload must not be null");
    }
    if (!sharingRoleRepository.existsByRoleId(roleId)) {
      throw new ResourceNotFoundException("roleId", String.valueOf(roleId));
    }
  }

  @Override
  protected Set<String> findTenantsForConfig(SharingRoleRequest request) {
    return sharingRoleRepository.findTenantsByRoleId(request.getRoleId());
  }

  @Override
  protected void saveSharingConfig(List<SharingRoleEntity> sharingRoleEntityList) {
    sharingRoleRepository.saveAll(sharingRoleEntityList);
  }

  @Override
  protected void deleteSharingConfig(UUID roleId) {
    sharingRoleRepository.deleteByRoleId(roleId);
  }

  @Override
  protected SharingRoleEntity createSharingConfigEntityFromRequest(SharingRoleRequest request, String tenantId) {
    return SharingRoleEntity.builder()
      .id(UUID.randomUUID())
      .roleId(request.getRoleId())
      .tenantId(tenantId)
      .isCapabilitiesShared(false)
      .isCapabilitySetsShared(false)
      .build();
  }

  @Override
  protected SharingRoleResponse createSharingConfigResponse(UUID createRolesPcId, UUID updateRolesPcId) {
    return new SharingRoleResponse()
      .createRolesPCId(createRolesPcId)
      .updateRolesPCId(updateRolesPcId);
  }

  @Override
  protected SharingRoleDeleteResponse createSharingConfigResponse(UUID publishRequestId) {
    return new SharingRoleDeleteResponse()
      .pcId(publishRequestId);
  }

  @Override
  protected ObjectNode updatePayload(SharingRoleRequest request, String sourceValue) {
    var payload = objectMapper.convertValue(getPayload(request), ObjectNode.class);
    return payload.set(TYPE, new TextNode(sourceValue));
  }

  @Override
  protected String getSourceValue(SourceValues sourceValue) {
    return sourceValue.getRoleValue();
  }
}
