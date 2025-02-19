package org.folio.consortia.service.impl;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.folio.consortia.domain.dto.PrimaryAffiliationEvent;
import org.folio.consortia.domain.dto.SyncUser;
import org.folio.consortia.domain.dto.TenantDetails.SetupStatusEnum;
import org.folio.consortia.domain.entity.TenantEntity;
import org.folio.consortia.domain.entity.UserTenantEntity;
import org.folio.consortia.repository.UserTenantRepository;
import org.folio.consortia.service.CreatePrimaryAffiliationService;
import org.folio.consortia.service.LockService;
import org.folio.consortia.service.PrimaryAffiliationService;
import org.folio.consortia.service.TenantService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@Log4j2
@RequiredArgsConstructor
public class CreatePrimaryAffiliationServiceImpl implements CreatePrimaryAffiliationService {

  private final TenantService tenantService;
  private final UserTenantRepository userTenantRepository;
  private final LockService lockService;
  private final PrimaryAffiliationService primaryAffiliationService;

  @Override
  @Transactional
  public void createPrimaryUserAffiliations(UUID consortiumId, String centralTenantId, String tenantId, List<SyncUser> users) {
    try {
      log.info("Start creating user primary affiliation for tenant {}", tenantId);
      lockService.lockTenantSetupWithinTransaction();
      TenantEntity tenantEntity = tenantService.getByTenantId(tenantId);
      createPrimaryUserAffiliations(consortiumId, centralTenantId, tenantId, users, tenantEntity);
    } catch (Exception e) {
      log.error("createPrimaryUserAffiliations:: error creating user primary affiliations", e);
      tenantService.updateTenantSetupStatus(tenantId, centralTenantId, SetupStatusEnum.FAILED);
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
        Optional<UserTenantEntity> userTenant = userTenantRepository.findByUserIdAndIsPrimaryTrue(UUID.fromString(user.getId()));

        if (userTenant.isPresent()) {
          log.info("createPrimaryUserAffiliations:: Primary affiliation already exists for tenant/user: {}/{}", tenantId, user.getUsername());
        } else {
          PrimaryAffiliationEvent primaryAffiliationEvent = createPrimaryAffiliationEvent(user, tenantId, centralTenantId, consortiumId);
          primaryAffiliationService.createPrimaryAffiliationInNewTransaction(consortiumId, centralTenantId, tenantEntity, primaryAffiliationEvent);
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
