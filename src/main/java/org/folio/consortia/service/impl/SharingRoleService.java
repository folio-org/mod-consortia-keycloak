package org.folio.consortia.service.impl;

import static org.folio.consortia.utils.TenantContextUtils.prepareContextForTenant;

import com.bettercloud.vault.json.JsonObject;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import feign.FeignException;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.ObjectUtils;
import org.folio.consortia.client.RolesClient;
import org.folio.consortia.domain.dto.PublicationRequest;
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
public class SharingRoleService extends BaseSharingService<SharingRoleRequest, SharingRoleResponse, SharingRoleDeleteResponse, SharingRoleEntity> {

  private static final String ID = "id";

  private final RolesClient rolesClient;
  private final FolioModuleMetadata moduleMetadata;
  private final SharingRoleRepository sharingRoleRepository;

  public SharingRoleService(TenantService tenantService, ConsortiumService consortiumService,
                            SystemUserScopedExecutionService systemUserScopedExecutionService,
                            PublicationService publicationService, FolioExecutionContext folioExecutionContext,
                            ObjectMapper parentObjectMapper, TaskExecutor asyncTaskExecutor, RolesClient rolesClient, FolioModuleMetadata moduleMetadata, SharingRoleRepository sharingRoleRepository) {
    super(tenantService, consortiumService, systemUserScopedExecutionService, publicationService,
      folioExecutionContext, parentObjectMapper, asyncTaskExecutor);
    this.rolesClient = rolesClient;
    this.moduleMetadata = moduleMetadata;
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
    return payload.get(ID).asText();
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
    return sharingRoleRepository.findTenantsByRoleName(request.getRoleName());
  }

  @Override
  protected void syncConfigWithTenants(Set<String> sharedConfigTenants, SharingRoleRequest request) {
    String roleName = request.getRoleName();
    String centralTenantId = folioExecutionContext.getTenantId();
    log.debug("syncConfig:: Trying to syncing sharing role table with roles table for role '{}' and tenant '{}'",
      request.getRoleName(), centralTenantId);

    if (sharingRoleRepository.existsByRoleNameAndTenantId(roleName, centralTenantId)) {
      log.info("syncConfig:: Role '{}' with central tenant '{}' already exists, Syncing with other tenants: {}",
        request.getRoleName(), centralTenantId, sharedConfigTenants);

      sharedConfigTenants.stream()
        .filter(tenantId -> !tenantId.equals(centralTenantId))
        .forEach(memberTenantId -> syncSharingRoleWithRoleInTenant(request, roleName, memberTenantId));
      return;
    }

    log.info("syncConfig:: Role '{}' not found, trying to sync with only central tenant '{}'" +
        " because role haven't shared with other tenants yet", request.getRoleId(), centralTenantId);
    syncSharingRoleWithRoleInTenant(request, roleName, centralTenantId);
  }

  private void syncSharingRoleWithRoleInTenant(SharingRoleRequest request, String roleName, String tenantId) {
    try (var ignored = new FolioExecutionContextSetter(prepareContextForTenant(tenantId, moduleMetadata, folioExecutionContext))) {
      String cqlQuery = String.format("name==%s", roleName);
      JsonObject role = rolesClient.getRolesByQuery(cqlQuery);
      UUID roleId = UUID.fromString(role.getString(ID));

      request.setRoleId(roleId);
      log.info("syncConfig:: Role '{}' is found in tenant '{}' but not found in sharing role table," +
        " creating new record in sharing table", roleId, tenantId);

      var sharingRoleEntity = createSharingConfigEntity(roleId, roleName, tenantId);
      sharingRoleRepository.save(sharingRoleEntity);
    } catch (FeignException.NotFound e) {
      log.info("syncConfig:: Role '{}' not found in tenant '{}' and sharing role table, No need to sync",
        roleName, tenantId);
    } catch (Exception e) {
      log.error("syncConfig:: Error while fetching roles", e);
    }
  }

  @Override
  protected void saveSharingConfig(List<SharingRoleEntity> sharingRoleEntityList) {
    sharingRoleRepository.saveAll(sharingRoleEntityList);
  }

  @Override
  protected void deleteSharingConfig(SharingRoleRequest request) {
    sharingRoleRepository.deleteByRoleName(request.getRoleName());
  }

  @Override
  protected PublicationRequest buildPublicationRequestForTenant(SharingRoleRequest request, String tenantId,
                                                                HttpMethod method) {
    var payload = objectMapper.convertValue(getPayload(request), ObjectNode.class);
    String url = request.getUrl();

    if (method.equals(HttpMethod.PUT) || method.equals(HttpMethod.DELETE)) {
      // roleId will be different for each tenant
      var tenantRoleId = sharingRoleRepository.findRoleIdByRoleNameAndTenantId(request.getRoleName(), tenantId);
      url += "/" + tenantRoleId;
      payload.put(ID, tenantRoleId.toString());
      log.info("buildPublicationRequestForTenant:: roleId '{}' was set to tenant '{}'", tenantRoleId, tenantId);
    }

    return new PublicationRequest()
      .method(method.toString())
      .url(url)
      .payload(payload)
      .tenants(Set.of(tenantId));
  }

  @Override
  protected SharingRoleEntity createSharingConfigEntityFromRequest(SharingRoleRequest request, String tenantId) {
    return createSharingConfigEntity(request.getRoleId(), request.getRoleName(), tenantId);
  }

  private SharingRoleEntity createSharingConfigEntity(UUID roleId, String roleName, String tenantId) {
    return SharingRoleEntity.builder()
      .id(UUID.randomUUID())
      .roleId(roleId)
      .roleName(roleName)
      .tenantId(tenantId)
      .isCapabilitiesShared(false)
      .isCapabilitySetsShared(false)
      .build();
  }

  @Override
  protected SharingRoleResponse createSharingConfigResponse(List<UUID> createPcIds, List<UUID> updatePcIds) {
    return new SharingRoleResponse()
      .createPCIds(createPcIds)
      .updatePCIds(updatePcIds);
  }

  @Override
  protected SharingRoleDeleteResponse createSharingConfigDeleteResponse(List<UUID> publishRequestIds) {
    return new SharingRoleDeleteResponse()
      .pcIds(publishRequestIds);
  }

  @Override
  protected ObjectNode updateSourcePayload(Object payload, String sourceValue) {
    var payloadNode = objectMapper.convertValue(payload, ObjectNode.class);
    return payloadNode.set(TYPE, new TextNode(sourceValue));
  }

  @Override
  protected String getSourceValue(SourceValues sourceValue) {
    return sourceValue.getRoleValue();
  }
}
