package org.folio.consortia.service.impl;

import static org.folio.consortia.utils.TenantContextUtils.prepareContextForTenant;

import com.bettercloud.vault.json.JsonObject;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import feign.FeignException;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.ObjectUtils;
import org.folio.consortia.client.RoleCapabilitiesClient;
import org.folio.consortia.client.RolesClient;
import org.folio.consortia.domain.dto.PublicationRequest;
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
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.scope.FolioExecutionContextSetter;
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

  private static final String ID = "id";
  private static final String ROLE_ID = "roleId";

  private final FolioModuleMetadata moduleMetadata;
  private final RolesClient rolesClient;
  private final RoleCapabilitiesClient roleCapabilitiesClient;
  private final SharingRoleRepository sharingRoleRepository;

  public SharingRoleCapabilityService(TenantService tenantService, ConsortiumService consortiumService,
                                      SystemUserScopedExecutionService systemUserScopedExecutionService,
                                      PublicationService publicationService,
                                      FolioExecutionContext folioExecutionContext, ObjectMapper parentObjectMapper,
                                      TaskExecutor asyncTaskExecutor, FolioModuleMetadata moduleMetadata, RolesClient rolesClient, RoleCapabilitiesClient roleCapabilitiesClient,
                                      SharingRoleRepository sharingRoleRepository) {
    super(tenantService, consortiumService, systemUserScopedExecutionService, publicationService,
      folioExecutionContext, parentObjectMapper, asyncTaskExecutor);
    this.moduleMetadata = moduleMetadata;
    this.rolesClient = rolesClient;
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
  protected String getSourceValue(SourceValues sourceValue) {
    return sourceValue.getRoleValue();
  }

  protected boolean shouldCompactRequests() {
    return false; // payloads are different for each tenant, so requests must not be compacted
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
  protected void syncConfigWithTenants(Set<String> sharedConfigTenants, SharingRoleCapabilityRequest request) {
    String roleName = request.getRoleName();
    String centralTenantId = folioExecutionContext.getTenantId();
    log.debug("syncConfigWithTenant:: Trying to syncing sharing role table with role capabilities for role '{}' in central tenant '{}'",
      request.getRoleName(), centralTenantId);

    if (sharingRoleRepository.existsByRoleNameAndTenantIdAndIsCapabilitiesSharedTrue(roleName, centralTenantId)) {
      log.info("syncConfigWithTenant:: Role '{}' and capabilities with tenant '{}' already exists, Syncing with other tenants: {}",
        request.getRoleName(), centralTenantId, sharedConfigTenants);

      sharedConfigTenants.stream()
        .filter(tenantId -> !tenantId.equals(centralTenantId))
        .forEach(memberTenantId -> syncSharingRoleWithRoleCapabilitiesInTenant(roleName, memberTenantId));
      return;
    }

    syncSharingRoleWithRoleCapabilitiesInTenant(roleName, centralTenantId);
  }

  private void syncSharingRoleWithRoleCapabilitiesInTenant(String roleName, String tenantId) {
    try (var ignored = new FolioExecutionContextSetter(prepareContextForTenant(tenantId, moduleMetadata, folioExecutionContext))) {
      String cqlQuery = String.format("name==%s", roleName);
      JsonObject roleObject = rolesClient.getRolesByQuery(cqlQuery);
      String roleId = roleObject.getString(ID);

      roleCapabilitiesClient.getRoleCapabilitiesByRoleId(roleName);
      log.info("syncConfigWithTenant:: Role '{}' and capabilities found in tenant '{}', but not found in sharing role table, " +
        " creating new record in sharing table", roleId, tenantId);

      var entity = getSharingRoleEntity(roleName, tenantId);
      entity.setRoleId(UUID.fromString(roleId)); // set new found roleId
      entity.setIsCapabilitiesShared(true);
      sharingRoleRepository.save(entity);
    } catch (FeignException.NotFound e) {
      log.info("syncSharingRoleWithRoleCapabilitiesInTenant:: Role '{}' and capabilities not found in tenant '{}'" +
        " and sharing role table, No need to sync", roleName, tenantId);
    }  catch (Exception e) {
      log.error("syncConfig:: Error while fetching role capabilities", e);
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
    return sharingRoleRepository.findByRoleNameAndTenantId(roleName, tenantId);
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
