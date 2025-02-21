package org.folio.consortia.service.impl;

import static org.apache.commons.lang3.ObjectUtils.isNotEmpty;
import static org.folio.consortia.service.impl.CustomFieldServiceImpl.ORIGINAL_TENANT_ID_CUSTOM_FIELD;
import static org.folio.consortia.service.impl.CustomFieldServiceImpl.ORIGINAL_TENANT_ID_NAME;
import static org.folio.consortia.utils.HelperUtils.checkIdenticalOrThrow;

import java.util.Objects;
import java.util.UUID;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.ObjectUtils;
import org.folio.consortia.client.ConsortiaConfigurationClient;
import org.folio.consortia.client.UserTenantsClient;
import org.folio.consortia.domain.dto.ConsortiaConfiguration;
import org.folio.consortia.domain.dto.Tenant;
import org.folio.consortia.domain.dto.TenantCollection;
import org.folio.consortia.domain.dto.TenantDeleteRequest;
import org.folio.consortia.domain.dto.TenantDeleteRequest.DeleteTypeEnum;
import org.folio.consortia.domain.dto.TenantDetails;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.dto.UserTenant;
import org.folio.consortia.domain.entity.TenantEntity;
import org.folio.consortia.exception.ResourceAlreadyExistException;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.service.CapabilitiesUserService;
import org.folio.consortia.service.CleanupService;
import org.folio.consortia.service.ConsortiumService;
import org.folio.consortia.service.CustomFieldService;
import org.folio.consortia.service.KeycloakService;
import org.folio.consortia.service.LockService;
import org.folio.consortia.service.SyncPrimaryAffiliationService;
import org.folio.consortia.service.TenantManager;
import org.folio.consortia.service.TenantService;
import org.folio.consortia.service.UserService;
import org.folio.consortia.utils.TenantContextUtils;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.context.ExecutionContextBuilder;
import org.folio.spring.scope.FolioExecutionContextSetter;
import org.folio.spring.service.SystemUserScopedExecutionService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Log4j2
@Service
@RequiredArgsConstructor
public class TenantManagerImpl implements TenantManager {

  private static final String SHADOW_ADMIN_PERMISSION_SETS_FILE_PATH = "permissions/admin-user-permission-sets.csv";
  private static final String TENANTS_IDS_NOT_MATCHED_ERROR_MSG = "Request body tenantId and path param tenantId should be identical";

  private static final String DUMMY_USERNAME = "dummy_user";

  private final TenantService tenantService;
  private final ConsortiumService consortiumService;
  private final ConsortiaConfigurationClient configurationClient;
  private final SyncPrimaryAffiliationService syncPrimaryAffiliationService;
  private final UserService userService;
  private final CapabilitiesUserService capabilitiesUserService;
  private final CustomFieldService customFieldService;
  private final CleanupService cleanupService;
  private final LockService lockService;
  private final UserTenantsClient userTenantsClient;
  private final SystemUserScopedExecutionService systemUserScopedExecutionService;
  private final ExecutionContextBuilder contextBuilder;
  private final FolioExecutionContext folioExecutionContext;
  private final KeycloakService keycloakService;


  @Override
  public TenantCollection get(UUID consortiumId, Integer offset, Integer limit) {
    return tenantService.get(consortiumId, offset, limit);
  }

  @Override
  @Transactional
  public Tenant save(UUID consortiumId, UUID adminUserId, Tenant tenantDto) {
    log.info("save:: Trying to save a tenant with id={}, consortiumId={} and isCentral={}", tenantDto.getId(),
      consortiumId, tenantDto.getIsCentral());
//    validateConsortiumAndTenantForSaveOperation(consortiumId, tenantDto);
    tenantService.checkTenantUniqueNameAndCodeOrThrow(tenantDto);

//    createCustomFieldIfNeeded(tenantDto.getId());k
    keycloakService.addCustomAuthFlowForCentralTenant(tenantDto);

    var existingTenant = tenantService.getByTenantId(tenantDto.getId());
    return existingTenant != null
      ? reAddSoftDeletedTenant(consortiumId, existingTenant, tenantDto)
      : addNewTenant(consortiumId, tenantDto, adminUserId);
  }

  @Override
  @Transactional
  public Tenant update(UUID consortiumId, String tenantId, Tenant tenantDto) {
    log.debug("update:: Trying to update tenant '{}' in consortium '{}'", tenantId, consortiumId);
    var existedTenant = getTenantById(tenantId);

    validateTenantForUpdateOperation(consortiumId, tenantId, tenantDto, existedTenant);
    // isDeleted flag cannot be changed by put request
    tenantDto.setIsDeleted(existedTenant.getIsDeleted());
    return tenantService.saveTenant(consortiumId, tenantDto);
  }

  @Override
  @Transactional
  public void delete(UUID consortiumId, String tenantId, TenantDeleteRequest tenantDeleteRequest) {
    log.info("delete:: Trying to delete tenant '{}' in consortium '{}'", tenantId, consortiumId);
    consortiumService.checkConsortiumExistsOrThrow(consortiumId);
    var tenant = getTenantById(tenantId);
    var deleteType = tenantDeleteRequest.getDeleteType();
    var deleteOptions = tenantDeleteRequest.getDeleteOptions();

    // Delete internal data only if it is a hard delete
    var isHardDelete = deleteType.equals(DeleteTypeEnum.HARD);
    // Delete users user-tenants always for soft delete and if deleteUsersUserTenants flag is set for hard delete

    validateTenantForDeleteOperation(tenantDeleteRequest.getDeleteType(), tenant);

    // Clean publish coordinator tables first, because after tenant removal it will be ignored by cleanup service
    cleanupService.clearPublicationTables();
    // Clean sharing tables or shadow users if needed
    if (isHardDelete) {
      cleanupService.clearSharingTables(tenantId);
    }
    tenantService.deleteTenant(tenant, tenantDeleteRequest.getDeleteType());

    var memberTenantContext = TenantContextUtils.prepareContextForTenant(tenantId, folioExecutionContext.getFolioModuleMetadata(), folioExecutionContext);
    try (var ignored = new FolioExecutionContextSetter(memberTenantContext)) {
      if (isHardDelete) {
        log.info("delete:: Deleting configuration for tenant '{}'", tenantId);
        configurationClient.deleteConfiguration();
      }
      // Delete user-tenants always for soft delete.
      // For hard delete, delete user-tenants if deleteUsersUserTenants flag is set and tenant is not already soft deleted
      if (!isHardDelete || deleteOptions.getDeleteUsersUserTenants() && !tenant.getIsDeleted()) {
        log.info("delete:: Deleting user-tenants for tenant '{}'", tenantId);
        userTenantsClient.deleteUserTenants();
      }
    }

    log.info("delete:: Tenant '{}' in consortium '{}' was successfully deleted", tenantId, consortiumId);
  }

  private void createCustomFieldIfNeeded(String tenant) {
    systemUserScopedExecutionService.executeSystemUserScoped(tenant, () -> {
        if (isNotEmpty(customFieldService.getCustomFieldByName(ORIGINAL_TENANT_ID_NAME))) {
          log.info("createOriginalTenantIdCustomField:: custom-field already available in tenant {} with name {}",
            tenant, ORIGINAL_TENANT_ID_NAME);
        } else {
          customFieldService.createCustomField(ORIGINAL_TENANT_ID_CUSTOM_FIELD);
        }
        return null;
      }
    );
  }

  private Tenant reAddSoftDeletedTenant(UUID consortiumId, TenantEntity existingTenant, Tenant tenantDto) {
    log.info("reAddSoftDeletedTenant:: Re-adding soft deleted tenant with id={}", tenantDto.getId());
    validateExistingTenant(existingTenant);

    tenantDto.setIsDeleted(false);
    var savedTenant = tenantService.saveTenantDetails(consortiumId, tenantDto, TenantDetails.SetupStatusEnum.COMPLETED);

    String centralTenantId = tenantService.getCentralTenantId();
    try (var ignored = new FolioExecutionContextSetter(contextBuilder.buildContext(tenantDto.getId()))) {
      if (isUserTenantsEmpty()) {
        createUserTenantWithDummyUser(tenantDto.getId(), centralTenantId, consortiumId);
        log.info("reAddSoftDeletedTenant:: Dummy user re-created in user-tenants table");
      }
    } catch (Exception e) {
      log.error("Failed to create dummy user with centralTenantId: {}, tenant: {}" +
        " and error message: {}", centralTenantId, tenantDto.getId(), e.getMessage(), e);
    }
    return savedTenant;
  }

  private Tenant addNewTenant(UUID consortiumId, Tenant tenantDto, UUID adminUserId) {
    log.info("addNewTenant:: Creating new tenant with id={}, consortiumId={}, and adminUserId={}",
      tenantDto.getId(), consortiumId, adminUserId);

    lockService.lockTenantSetupWithinTransaction();
    tenantDto.setIsDeleted(false);
    Tenant savedTenant = tenantService.saveTenantDetails(consortiumId, tenantDto, TenantDetails.SetupStatusEnum.IN_PROGRESS);

    // save admin user tenant association for non-central tenant
    String centralTenantId;
    User shadowAdminUser = null;
    if (tenantDto.getIsCentral()) {
      centralTenantId = tenantDto.getId();
    } else {
      checkAdminUserIdPresentOrThrow(adminUserId);
      centralTenantId = tenantService.getCentralTenantId();
      shadowAdminUser = userService.prepareShadowUser(adminUserId, folioExecutionContext.getTenantId());
      tenantService.saveUserTenant(consortiumId, shadowAdminUser, tenantDto);
    }

    var finalShadowAdminUser = shadowAdminUser;
    // switch to context of the desired tenant and apply all necessary setup
    try (var ignored = new FolioExecutionContextSetter(TenantContextUtils.prepareContextForTenant(tenantDto.getId(),
      folioExecutionContext.getFolioModuleMetadata(), folioExecutionContext))) {
      // Use self-invocation to avoid making calls to other DB schemas and prevent issues
      // with SQL connections tied to the original tenant gathered from connection pool, which would prevent tenant switching
      configurationClient.saveConfiguration(createConsortiaConfigurationBody(centralTenantId));
      log.info("save:: consortia configuration was created in tenant '{}'", tenantDto.getId());
      if (!tenantDto.getIsCentral() && isUserTenantsEmpty()) {
        createUserTenantWithDummyUser(tenantDto.getId(), centralTenantId, consortiumId);
        createShadowAdminWithPermissions(finalShadowAdminUser);
        log.info("save:: shadow admin user '{}' with permissions was created in tenant '{}'", finalShadowAdminUser.getId(), tenantDto.getId());
      }

      syncPrimaryAffiliationService.syncPrimaryAffiliations(consortiumId, tenantDto.getId(), centralTenantId);
    }
    log.info("save:: saved consortia configuration with centralTenantId={} by tenantId={} context", centralTenantId, tenantDto.getId());
    return savedTenant;
  }

  private TenantEntity getTenantById(String tenantId) {
    var tenant = tenantService.getByTenantId(tenantId);
    if (Objects.isNull(tenant)) {
      throw new ResourceNotFoundException("id", tenantId);
    }
    return tenant;
  }

  private boolean isUserTenantsEmpty() {
    return userTenantsClient.getUserTenants().getTotalRecords().equals(0);
  }

  /**
   * Dummy user will be used to support cross-tenant requests checking in mod-authtoken,
   * if user-tenant table contains some record in institutional tenant - it means mod-consortia enabled for
   * this tenant and will allow cross-tenant request.
   *
   * @param tenantId        tenant id
   * @param centralTenantId central tenant id
   * @param consortiumId    consortium id
   */
  private void createUserTenantWithDummyUser(String tenantId, String centralTenantId, UUID consortiumId) {
    UserTenant userTenant = new UserTenant();
    userTenant.setId(UUID.randomUUID());
    userTenant.setTenantId(tenantId);
    userTenant.setUserId(UUID.randomUUID());
    userTenant.setUsername(DUMMY_USERNAME);
    userTenant.setCentralTenantId(centralTenantId);
    userTenant.setConsortiumId(consortiumId);

    log.info("Creating userTenant with dummy user with id {}.", userTenant.getId());
    userTenantsClient.postUserTenant(userTenant);
  }

  private void validateTenantForUpdateOperation(UUID consortiumId, String tenantId, Tenant tenantDto, TenantEntity existedTenant) {
    consortiumService.checkConsortiumExistsOrThrow(consortiumId);
    checkIdenticalOrThrow(tenantId, tenantDto.getId(), TENANTS_IDS_NOT_MATCHED_ERROR_MSG);
    if (ObjectUtils.notEqual(tenantDto.getIsCentral(), existedTenant.getIsCentral())) {
      throw new IllegalArgumentException(String.format("'isCentral' field cannot be changed. It should be '%s'", existedTenant.getIsCentral()));
    }
  }

  private void validateConsortiumAndTenantForSaveOperation(UUID consortiumId, Tenant tenantDto) {
    consortiumService.checkConsortiumExistsOrThrow(consortiumId);
    if (tenantDto.getIsCentral() && tenantService.centralTenantExists()) {
      throw new ResourceAlreadyExistException("isCentral", "true");
    }
  }

  private void validateExistingTenant(TenantEntity existingTenant) {
    if (Boolean.FALSE.equals(existingTenant.getIsDeleted())) {
      throw new ResourceAlreadyExistException("id", existingTenant.getId());
    }
  }

  // During soft delete central or already deleted tenant cannot proceed
  private void validateTenantForDeleteOperation(DeleteTypeEnum deleteType, TenantEntity tenant) {
    if (DeleteTypeEnum.HARD.equals(deleteType)) {
      return;
    }
    if (Boolean.TRUE.equals(tenant.getIsDeleted())) {
      throw new IllegalArgumentException(String.format("Tenant [%s] has already been soft deleted.", tenant.getId()));
    }
    if (Boolean.TRUE.equals(tenant.getIsCentral())) {
      throw new IllegalArgumentException(String.format("Central tenant [%s] cannot be deleted.", tenant.getId()));
    }
  }

  private void checkAdminUserIdPresentOrThrow(UUID adminUserId) {
    if (Objects.isNull(adminUserId)) {
      log.warn("checkAdminUserIdPresentOrThrow:: adminUserId is not present");
      throw new IllegalArgumentException("Required request parameter 'adminUserId' for method parameter type UUID is not present");
    }
  }

  private void createShadowAdminWithPermissions(User user) {
    User userOptional = userService.getById(UUID.fromString(user.getId()));
    if (Objects.isNull(userOptional.getId())) {
      userOptional = userService.createUser(user);
    }
    capabilitiesUserService.createWithPermissionSetsFromFile(userOptional.getId(), SHADOW_ADMIN_PERMISSION_SETS_FILE_PATH);
  }

  private ConsortiaConfiguration createConsortiaConfigurationBody(String tenantId) {
    ConsortiaConfiguration configuration = new ConsortiaConfiguration();
    configuration.setCentralTenantId(tenantId);
    return configuration;
  }

}
