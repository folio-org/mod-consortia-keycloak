package org.folio.consortia.service.impl;

import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.SharingSettingRepository;

import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

import org.apache.commons.lang3.ObjectUtils;
import org.folio.consortia.domain.dto.PublicationRequest;
import org.folio.consortia.domain.dto.SharingSettingDeleteResponse;
import org.folio.consortia.domain.dto.SharingSettingRequest;
import org.folio.consortia.domain.dto.SharingSettingResponse;
import org.folio.consortia.domain.entity.SharingSettingEntity;
import org.folio.consortia.service.BaseSharingService;
import org.folio.consortia.service.ConsortiumService;
import org.folio.consortia.service.PublicationService;
import org.folio.consortia.service.TenantService;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.service.SystemUserScopedExecutionService;
import org.springframework.core.task.TaskExecutor;
import org.springframework.http.HttpMethod;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;

import lombok.extern.log4j.Log4j2;

@Service
@Log4j2
public class SharingSettingService extends BaseSharingService<SharingSettingRequest, SharingSettingResponse, SharingSettingDeleteResponse, SharingSettingEntity> {

  private static final String SOURCE = "source";

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
  protected UUID getConfigId(SharingSettingRequest sharingSettingRequest) {
    return sharingSettingRequest.getSettingId();
  }

  @Override
  protected Object getPayload(SharingSettingRequest sharingSettingRequest) {
    return sharingSettingRequest.getPayload();
  }

  @Override
  protected void validateSharingConfigRequestOrThrow(UUID settingId, SharingSettingRequest sharingSettingRequest) {
    if (ObjectUtils.notEqual(getConfigId(sharingSettingRequest), settingId)) {
      throw new IllegalArgumentException("Mismatch id in path to settingId in request body");
    }
    if (Objects.isNull(getPayload(sharingSettingRequest))) {
      throw new IllegalArgumentException("Payload must not be null");
    }
    if (!sharingSettingRepository.existsBySettingId(settingId)) {
      throw new ResourceNotFoundException("settingId", String.valueOf(settingId));
    }
  }

  @Override
  protected Set<String> findTenantsByConfigId(UUID settingId) {
    return sharingSettingRepository.findTenantsBySettingId(settingId);
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
  protected PublicationRequest createPublicationRequest(SharingSettingRequest sharingSettingRequest, String httpMethod) {
    PublicationRequest publicationRequest = new PublicationRequest();
    publicationRequest.setMethod(httpMethod);
    String url = sharingSettingRequest.getUrl();
    if (httpMethod.equals(HttpMethod.PUT.toString()) || httpMethod.equals(HttpMethod.DELETE.toString())) {
      url += "/" + getConfigId(sharingSettingRequest);
    }
    publicationRequest.setUrl(url);
    publicationRequest.setPayload(getPayload(sharingSettingRequest));
    publicationRequest.setTenants(new HashSet<>());
    return publicationRequest;
  }

  @Override
  protected SharingSettingEntity createSharingConfigEntityFromRequest(SharingSettingRequest sharingSettingRequest, String tenantId) {
    SharingSettingEntity sharingSettingEntity = new SharingSettingEntity();
    sharingSettingEntity.setId(UUID.randomUUID());
    sharingSettingEntity.setSettingId(sharingSettingRequest.getSettingId());
    sharingSettingEntity.setTenantId(tenantId);
    return sharingSettingEntity;
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
  protected ObjectNode updatePayload(SharingSettingRequest sharingConfigRequest, String sourceValue) {
    JsonNode payload = objectMapper.convertValue(getPayload(sharingConfigRequest), JsonNode.class);
    return ((ObjectNode) payload).set(SOURCE, new TextNode(sourceValue));
  }

}
