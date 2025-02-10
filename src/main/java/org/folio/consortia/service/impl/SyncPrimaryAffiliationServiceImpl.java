package org.folio.consortia.service.impl;

import static org.folio.spring.scope.FolioExecutionScopeExecutionContextManager.getRunnableWithFolioContext;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.consortia.domain.dto.Personal;
import org.folio.consortia.domain.dto.SyncPrimaryAffiliationBody;
import org.folio.consortia.domain.dto.SyncUser;
import org.folio.consortia.domain.dto.TenantDetails.SetupStatusEnum;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.service.CreatePrimaryAffiliationService;
import org.folio.consortia.service.SyncPrimaryAffiliationService;
import org.folio.consortia.service.TenantService;
import org.folio.consortia.service.UserService;
import org.folio.consortia.utils.TenantContextUtils;
import org.folio.spring.FolioExecutionContext;
import org.springframework.core.task.AsyncTaskExecutor;
import org.springframework.stereotype.Service;

@Service
@Log4j2
@RequiredArgsConstructor
public class SyncPrimaryAffiliationServiceImpl implements SyncPrimaryAffiliationService {

  private final UserService userService;
  private final TenantService tenantService;
  private final CreatePrimaryAffiliationService createPrimaryAffiliationService;
  private final FolioExecutionContext folioExecutionContext;
  private final AsyncTaskExecutor asyncTaskExecutor;

  @Override
  public void syncPrimaryAffiliations(UUID consortiumId, String tenantId, String centralTenantId) {
    var context = TenantContextUtils.prepareContextForTenant(tenantId, folioExecutionContext.getFolioModuleMetadata(), folioExecutionContext);
    asyncTaskExecutor.execute(getRunnableWithFolioContext(context,
      () -> syncPrimaryAffiliationsInternal(consortiumId, tenantId, centralTenantId)));
  }

  @Override
  public void syncPrimaryUserAffiliations(UUID consortiumId, String centralTenantId, SyncPrimaryAffiliationBody syncPrimaryAffiliationBody) {
    var context = TenantContextUtils.prepareContextForTenant(centralTenantId, folioExecutionContext.getFolioModuleMetadata(), folioExecutionContext);
    asyncTaskExecutor.execute(getRunnableWithFolioContext(context,
      () -> createPrimaryAffiliationService.createPrimaryUserAffiliations(consortiumId, centralTenantId, syncPrimaryAffiliationBody.getTenantId(), syncPrimaryAffiliationBody.getUsers())));
  }

  void syncPrimaryAffiliationsInternal(UUID consortiumId, String tenantId, String centralTenantId) {
    log.info("Start syncing user primary affiliations for tenant {}", tenantId);
    List<User> users = new ArrayList<>();
    try {
      users = userService.getUsersByQuery("(cql.allRecords=1 NOT type=\"patron\" NOT type=\"dcb\" NOT type=\"shadow\" NOT type=\"system\")", 0, Integer.MAX_VALUE);
    } catch (Exception e) {
      log.error("syncPrimaryAffiliations:: failed to retrieve '{}' users", tenantId, e);
      tenantService.updateTenantSetupStatus(tenantId, centralTenantId, SetupStatusEnum.FAILED);
    }

    if (CollectionUtils.isNotEmpty(users)) {
      try {
        this.syncPrimaryUserAffiliations(consortiumId, centralTenantId,  buildSyncPrimaryAffiliationBody(tenantId, users));
      } catch (Exception e) {
        log.error("syncPrimaryAffiliations:: error syncing user primary affiliations", e);
        tenantService.updateTenantSetupStatus(tenantId, centralTenantId, SetupStatusEnum.FAILED);
        throw e;
      }
    }
  }

  private SyncPrimaryAffiliationBody buildSyncPrimaryAffiliationBody(String tenantId, List<User> users) {
    return new SyncPrimaryAffiliationBody()
      .tenantId(tenantId)
      .users(users.stream()
        .map(user -> getSyncUser(user, tenantId))
        .toList());
  }

  private SyncUser getSyncUser(User user, String tenantId) {
    SyncUser syncUser = new SyncUser()
      .id(user.getId())
      .username(user.getUsername())
      .externalSystemId(user.getExternalSystemId())
      .barcode(user.getBarcode());
    Personal personal = user.getPersonal();
    if (Objects.nonNull(personal)) {
      syncUser
        .email(personal.getEmail())
        .phoneNumber(personal.getPhone())
        .mobilePhoneNumber(personal.getMobilePhone());
    }
    if (StringUtils.isBlank(user.getType())) {
      log.warn("Required field 'type' was not populated for existing user with id: {}, username: {} in tenant: {}",
        user.getId(), user.getUsername(), tenantId);
    }
    return syncUser;
  }

}
