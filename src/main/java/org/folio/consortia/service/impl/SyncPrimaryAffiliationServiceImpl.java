package org.folio.consortia.service.impl;

import static org.folio.spring.scope.FolioExecutionScopeExecutionContextManager.getRunnableWithCurrentFolioContext;
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
import org.folio.consortia.domain.dto.PrimaryAffiliationEvent;
import org.folio.consortia.domain.dto.SyncPrimaryAffiliationBody;
import org.folio.consortia.domain.dto.SyncUser;
import org.folio.consortia.domain.dto.TenantDetails.SetupStatusEnum;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.entity.TenantEntity;
import org.folio.consortia.domain.entity.UserTenantEntity;
import org.folio.consortia.repository.UserTenantRepository;
import org.folio.consortia.service.LockService;
import org.folio.consortia.service.PrimaryAffiliationService;
import org.folio.consortia.service.SyncPrimaryAffiliationService;
import org.folio.consortia.service.TenantService;
import org.folio.consortia.service.UserService;
import org.folio.consortia.utils.TenantContextUtils;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.data.OffsetRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.core.task.AsyncTaskExecutor;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Log4j2
@RequiredArgsConstructor
public class SyncPrimaryAffiliationServiceImpl implements SyncPrimaryAffiliationService {
  private final UserService userService;
  private final TenantService tenantService;
  private final UserTenantRepository userTenantRepository;
  private final LockService lockService;
  private final PrimaryAffiliationService createPrimaryAffiliationService;
  private final FolioExecutionContext folioExecutionContext;
  private final AsyncTaskExecutor asyncTaskExecutor;

  // Self reference to enable @Transactional method calls
  private SyncPrimaryAffiliationServiceImpl self;
  @Autowired
  public void setSyncPrimaryAffiliationService(@Lazy SyncPrimaryAffiliationServiceImpl self) {
    this.self = self;
  }

  @Override
  public void syncPrimaryAffiliations(UUID consortiumId, String tenantId, String centralTenantId) {
    asyncTaskExecutor.execute(getRunnableWithCurrentFolioContext(
      () -> syncPrimaryAffiliationsInternal(consortiumId, tenantId, centralTenantId)));
  }

  void syncPrimaryAffiliationsInternal(UUID consortiumId, String tenantId, String centralTenantId) {
    log.info("Start syncing user primary affiliations for tenant {}", tenantId);
    List<User> users = new ArrayList<>();
    try {
      users = userService.getUsersByQuery("(cql.allRecords=1 NOT type=\"patron\" NOT type=\"dcb\" NOT type=\"shadow\")", 0, Integer.MAX_VALUE);
    } catch (Exception e) {
      log.error("syncPrimaryAffiliations:: failed to retrieve '{}' users", tenantId, e);
      tenantService.updateTenantSetupStatus(tenantId, centralTenantId, SetupStatusEnum.FAILED);
    }

    if (CollectionUtils.isNotEmpty(users)) {
      try {
          self.createPrimaryUserAffiliations(consortiumId, centralTenantId,  buildSyncPrimaryAffiliationBody(tenantId, users));
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

  @Override
  public void createPrimaryUserAffiliations(UUID consortiumId, String centralTenantId, SyncPrimaryAffiliationBody syncPrimaryAffiliationBody) {
    var context = TenantContextUtils.prepareContextForTenant(centralTenantId, folioExecutionContext.getFolioModuleMetadata(), folioExecutionContext);
    asyncTaskExecutor.execute(getRunnableWithFolioContext(context,
      () -> self.createPrimaryUserAffiliationsInternal(consortiumId, centralTenantId, syncPrimaryAffiliationBody)));
  }

  @Transactional
  void createPrimaryUserAffiliationsInternal(UUID consortiumId, String centralTenantId, SyncPrimaryAffiliationBody syncPrimaryAffiliationBody) {
    try {
      log.info("Start creating user primary affiliation for tenant {}", syncPrimaryAffiliationBody.getTenantId());
      lockService.lockTenantSetupWithinTransaction();
      var tenantId = syncPrimaryAffiliationBody.getTenantId();
      var userList = syncPrimaryAffiliationBody.getUsers();
      TenantEntity tenantEntity = tenantService.getByTenantId(tenantId);
      createPrimaryUserAffiliations(consortiumId, centralTenantId, tenantId, userList, tenantEntity);
    } catch (Exception e) {
      log.error("createPrimaryUserAffiliations:: error creating user primary affiliations", e);
      tenantService.updateTenantSetupStatus(syncPrimaryAffiliationBody.getTenantId(), centralTenantId, SetupStatusEnum.FAILED);
      throw e;
    }
  }

  private void createPrimaryUserAffiliations(UUID consortiumId, String centralTenantId, String tenantId,
    List<SyncUser> userList, TenantEntity tenantEntity) {
    var affiliatedUsersCount = 0;
    var hasFailedAffiliations = false;
    for (int idx = 0; idx < userList.size(); idx++) {
      var user = userList.get(idx);
      try {
        log.info("createPrimaryUserAffiliations:: Processing users: {} of {}", idx + 1, userList.size());
        Page<UserTenantEntity> userTenantPage = userTenantRepository.findAnyByUserId(UUID.fromString(user.getId()), OffsetRequest.of(0, 1));

        if (userTenantPage.getTotalElements() > 0) {
          log.info("createPrimaryUserAffiliations:: Primary affiliation already exists for tenant/user: {}/{}",
            tenantId, user.getUsername());
        } else {
          PrimaryAffiliationEvent primaryAffiliationEvent = createPrimaryAffiliationEvent(user, tenantId, centralTenantId, consortiumId);
          createPrimaryAffiliationService.createPrimaryAffiliationInNewTransaction(consortiumId, centralTenantId, tenantEntity, primaryAffiliationEvent);
        }
        affiliatedUsersCount++;
      } catch (Exception e) {
        hasFailedAffiliations = true;
        log.error("createPrimaryUserAffiliations:: Failed to create primary affiliations for userid: {}, tenant: {}" +
          " and error message: {}", user.getId(), tenantId, e.getMessage(), e);
      }
    }
    tenantService.updateTenantSetupStatus(tenantId, centralTenantId, hasFailedAffiliations ?
      SetupStatusEnum.COMPLETED_WITH_ERRORS : SetupStatusEnum.COMPLETED);
    log.info("createPrimaryUserAffiliations:: Successfully created {} of {} primary affiliations for tenant {}",
      affiliatedUsersCount, userList.size(), tenantId);
  }

  private PrimaryAffiliationEvent createPrimaryAffiliationEvent(SyncUser user,
                                                                String tenantId,
                                                                String centralTenantId,
                                                                UUID consortiumId) {
    PrimaryAffiliationEvent event = new PrimaryAffiliationEvent();
    event.setId(UUID.randomUUID());
    event.setUserId(UUID.fromString(user.getId()));
    event.setUsername(user.getUsername());
    event.setTenantId(tenantId);
    event.setEmail(user.getEmail());
    event.setPhoneNumber(user.getPhoneNumber());
    event.setMobilePhoneNumber(user.getMobilePhoneNumber());
    event.setBarcode(user.getBarcode());
    event.setExternalSystemId(user.getExternalSystemId());
    event.setCentralTenantId(centralTenantId);
    event.setConsortiumId(consortiumId);
    return event;
  }
}
