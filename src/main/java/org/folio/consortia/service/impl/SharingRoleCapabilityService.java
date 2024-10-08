package org.folio.consortia.service.impl;


import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;

import feign.FeignException;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.folio.consortia.client.RoleCapabilitiesClient;
import org.folio.consortia.domain.dto.PublicationRequest;
import org.folio.consortia.domain.dto.SharingRoleCapabilityDeleteResponse;
import org.folio.consortia.domain.dto.SharingRoleCapabilityRequest;
import org.folio.consortia.domain.dto.SharingRoleCapabilityResponse;
import org.folio.consortia.domain.dto.SharingRoleRequest;
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

@Service
@Log4j2
public class SharingRoleCapabilityService extends BaseSharingService<SharingRoleCapabilityRequest,
  SharingRoleCapabilityResponse, SharingRoleCapabilityDeleteResponse, SharingRoleEntity> {

  private static final String ROLE_ID = "roleId";

  private final RoleCapabilitiesClient roleCapabilitiesClient;
  private final SharingRoleRepository sharingRoleRepository;
  private final SharingRoleService sharingRoleService;

  public SharingRoleCapabilityService(TenantService tenantService, ConsortiumService consortiumService,
                                      SystemUserScopedExecutionService systemUserScopedExecutionService,
                                      PublicationService publicationService,
                                      FolioExecutionContext folioExecutionContext, ObjectMapper parentObjectMapper,
                                      TaskExecutor asyncTaskExecutor, RoleCapabilitiesClient roleCapabilitiesClient,
                                      SharingRoleRepository sharingRoleRepository, SharingRoleService sharingRoleService) {
    super(tenantService, consortiumService, systemUserScopedExecutionService, publicationService,
      folioExecutionContext, parentObjectMapper, asyncTaskExecutor);
    this.roleCapabilitiesClient = roleCapabilitiesClient;
    this.sharingRoleRepository = sharingRoleRepository;
    this.sharingRoleService = sharingRoleService;
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
  protected String getSourceValue(SourceValues sourceValue) {
    return sourceValue.getRoleValue();
  }

  /**
   * RoleId is generated by keycloak that will be used for role-capabilities, so payloads are different for each tenant,
   * that is why requests must not be compacted
   */
  @Override
  protected boolean shouldCompactRequests() {
    return false;
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
  protected void syncConfigWithTenants(SharingRoleCapabilityRequest request) {
    var sharingRoleRequest = new SharingRoleRequest();
    sharingRoleRequest.setRoleId(request.getRoleId());
    sharingRoleRequest.setRoleName(request.getRoleName());
    sharingRoleService.syncConfigWithTenants(sharingRoleRequest);

    log.debug("syncConfigWithTenant:: Trying to syncing sharing role table with role capabilities for role '{}' in tenants'",
      request.getRoleName());

    syncSharingRoleCapabilityForTenant(request);
  }

  private void syncSharingRoleCapabilityForTenant(SharingRoleCapabilityRequest request) {
    findTenantsForConfig(request).stream()
      .filter(tenantId ->
        !sharingRoleRepository.existsByRoleNameAndTenantIdAndIsCapabilitiesSharedTrue(request.getRoleName(), tenantId))
      .forEach(tenantId ->
        systemUserScopedExecutionService.executeSystemUserScoped(tenantId, () -> {
          syncSharingRoleCapabilityForTenant(request.getRoleName(), tenantId);
          return null;
        }));
  }

  private void syncSharingRoleCapabilityForTenant(String roleName, String tenantId) {
    try {
      var existingRole = sharingRoleRepository.findByRoleNameAndTenantId(roleName, tenantId);

      if (existingRole.isEmpty()) {
        log.info("syncSharingRoleWithRoleCapabilitiesInTenant:: Role '{}' not found in tenant '{}'" +
          " and sharing role table, No need to sync", roleName, tenantId);
        return;
      }

      var roleIdForTenant = existingRole.get().getRoleId();
      var capabilities = roleCapabilitiesClient.getRoleCapabilitiesByRoleId(roleName);
      if (CollectionUtils.isEmpty(capabilities.getCapabilities())) {
        log.info("syncConfigWithTenant:: No capabilitySets found for role '{}' in tenant '{}'" +
          "No need to sync", roleIdForTenant, tenantId);
        return;
      }

      log.info("syncConfigWithTenant:: Role '{}' and capabilities found in tenant '{}', but not found in sharing role table, " +
        " creating new record in sharing table", roleIdForTenant, tenantId);

      var entity = getSharingRoleEntity(roleName, tenantId);
      entity.setIsCapabilitiesShared(true);
      sharingRoleRepository.save(entity);
    } catch (FeignException.NotFound e) {
      log.info("syncSharingRoleWithRoleCapabilitiesInTenant:: Role '{}' and capabilities not found in tenant '{}'" +
        " and sharing role table, No need to sync", roleName, tenantId);
    } catch (Exception e) {
      log.error("syncConfig:: Error while fetching role capabilities", e);
      throw new IllegalStateException("Error while fetching role capabilities", e);
    }
  }

  @Override
  protected Set<String> findTenantsForConfig(SharingRoleCapabilityRequest request) {
    return sharingRoleRepository.findTenantsByRoleNameAndIsCapabilitiesSharedTrue(request.getRoleName());
  }

  @Override
  protected void saveSharingConfig(List<SharingRoleEntity> sharingRoleEntityList) {
    sharingRoleEntityList.forEach(sharingRoleEntity -> sharingRoleEntity.setIsCapabilitiesShared(true));
    sharingRoleRepository.saveAll(sharingRoleEntityList);
  }

  @Override
  protected void deleteSharingConfig(SharingRoleCapabilityRequest request) {
    var sharingRoleEntityList = sharingRoleRepository.findByRoleName(request.getRoleName());
    sharingRoleEntityList.forEach(sharingRoleEntity -> sharingRoleEntity.setIsCapabilitiesShared(false));
    sharingRoleRepository.saveAll(sharingRoleEntityList);
  }

  @Override
  protected PublicationRequest buildPublicationRequestForTenant(SharingRoleCapabilityRequest request,
                                                                String tenantId, HttpMethod method) {
    var payload = objectMapper.convertValue(getPayload(request), ObjectNode.class);
    String url = request.getUrl();
    if (method.equals(HttpMethod.PUT) || method.equals(HttpMethod.DELETE)) { // roleId will be different for each tenant
      var tenantRoleId = sharingRoleRepository.findRoleIdByRoleNameAndTenantId(request.getRoleName(), tenantId);
      url = url.replace("capabilities", tenantRoleId + "/capabilities");
      payload.put(ROLE_ID, tenantRoleId.toString());
      log.info("buildPublicationRequestForTenant:: roleId '{}' was sent to tenant '{}'", tenantRoleId, tenantId);
    }

    return new PublicationRequest()
      .method(method.toString())
      .url(url)
      .payload(payload)
      .tenants(Set.of(tenantId));
  }

  @Override
  protected SharingRoleEntity createSharingConfigEntityFromRequest(SharingRoleCapabilityRequest request,
                                                                   String tenantId) {
    return getSharingRoleEntity(request.getRoleName(), tenantId);
  }

  private SharingRoleEntity getSharingRoleEntity(String roleName, String tenantId) {
    return sharingRoleRepository.findByRoleNameAndTenantId(roleName, tenantId)
      .orElseThrow(() -> new ResourceNotFoundException("sharing role, tenantId", roleName + ", " + tenantId));
  }

  @Override
  protected SharingRoleCapabilityResponse createSharingConfigResponse(List<UUID> createPCIds,
                                                                      List<UUID> updatePCIds) {
    return new SharingRoleCapabilityResponse()
      .createPCIds(createPCIds)
      .updatePCIds(updatePCIds);
  }

  @Override
  protected SharingRoleCapabilityDeleteResponse createSharingConfigDeleteResponse(List<UUID> publishRequestId) {
    return new SharingRoleCapabilityDeleteResponse()
      .pcIds(publishRequestId);
  }

  @Override
  protected ObjectNode updateSourcePayload(Object payload, String sourceValue) {
    var node = objectMapper.convertValue(payload, ObjectNode.class);
    return node.set(TYPE, new TextNode(sourceValue));
  }
}
