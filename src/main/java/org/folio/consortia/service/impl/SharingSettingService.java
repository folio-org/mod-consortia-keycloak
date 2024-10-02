package org.folio.consortia.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.ObjectUtils;
import org.folio.consortia.domain.dto.SharingSettingDeleteResponse;
import org.folio.consortia.domain.dto.SharingSettingRequest;
import org.folio.consortia.domain.dto.SharingSettingResponse;
import org.folio.consortia.domain.dto.SourceValues;
import org.folio.consortia.domain.entity.SharingSettingEntity;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.SharingSettingRepository;
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
public class SharingSettingService extends BaseSharingService<SharingSettingRequest, SharingSettingResponse, SharingSettingDeleteResponse, SharingSettingEntity> {

  private final SharingSettingRepository sharingSettingRepository;

  public SharingSettingService(TenantService tenantService, ConsortiumService consortiumService,
                               SystemUserScopedExecutionService systemUserScopedExecutionService,
                               PublicationService publicationService, FolioExecutionContext folioExecutionContext,
                               ObjectMapper parentObjectMapper, TaskExecutor asyncTaskExecutor, SharingSettingRepository sharingSettingRepository) {
    super(tenantService, consortiumService, systemUserScopedExecutionService, publicationService,
      folioExecutionContext, parentObjectMapper, asyncTaskExecutor);
    this.sharingSettingRepository = sharingSettingRepository;
  }

  @Override
  protected UUID getConfigId(SharingSettingRequest request) {
    return request.getSettingId();
  }

  @Override
  protected Object getPayload(SharingSettingRequest request) {
    return request.getPayload();
  }

  @Override
  protected String getPayloadId(ObjectNode payload) {
    return payload.get("id").asText();
  }

  @Override
  protected String getUrl(SharingSettingRequest request, HttpMethod httpMethod) {
    String url = request.getUrl();
    if (httpMethod.equals(HttpMethod.PUT) || httpMethod.equals(HttpMethod.DELETE)) {
      url += "/" + getConfigId(request);
    }
    return url;
  }

  @Override
  protected void validateSharingConfigRequestOrThrow(UUID settingId, SharingSettingRequest request) {
    if (ObjectUtils.notEqual(getConfigId(request), settingId)) {
      throw new IllegalArgumentException("Mismatch id in path to settingId in request body");
    }
    if (Objects.isNull(getPayload(request))) {
      throw new IllegalArgumentException("Payload must not be null");
    }
    if (!sharingSettingRepository.existsBySettingId(settingId)) {
      throw new ResourceNotFoundException("settingId", String.valueOf(settingId));
    }
  }

  @Override
  protected Set<String> findTenantsForConfig(SharingSettingRequest request) {
    return sharingSettingRepository.findTenantsBySettingId(request.getSettingId());
  }

  @Override
  protected void saveSharingConfig(List<SharingSettingEntity> sharingSettingEntityList) {
    sharingSettingRepository.saveAll(sharingSettingEntityList);
  }

  @Override
  protected void deleteSharingConfig(UUID settingId) {
    sharingSettingRepository.deleteBySettingId(settingId);
  }

  @Override
  protected SharingSettingEntity createSharingConfigEntityFromRequest(SharingSettingRequest request, String tenantId) {
    return SharingSettingEntity.builder()
      .id(UUID.randomUUID())
      .settingId(request.getSettingId())
      .tenantId(tenantId)
      .build();
  }

  @Override
  protected SharingSettingResponse createSharingConfigResponse(UUID createSettingsPcId, UUID updateSettingsPcId) {
    return new SharingSettingResponse()
      .createSettingsPCId(createSettingsPcId)
      .updateSettingsPCId(updateSettingsPcId);
  }

  @Override
  protected SharingSettingDeleteResponse createSharingConfigResponse(UUID publishRequestId) {
    return new SharingSettingDeleteResponse()
      .pcId(publishRequestId);
  }

  @Override
  protected ObjectNode updatePayload(SharingSettingRequest request, String sourceValue) {
    var payload = objectMapper.convertValue(getPayload(request), ObjectNode.class);
    return payload.set(SOURCE, new TextNode(sourceValue));
  }

  @Override
  protected String getSourceValue(SourceValues sourceValue) {
    return sourceValue.getSettingValue();
  }
}
