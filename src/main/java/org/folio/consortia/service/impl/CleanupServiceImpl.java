package org.folio.consortia.service.impl;

import org.folio.consortia.repository.ConsortiumRepository;
import org.folio.consortia.repository.PublicationStatusRepository;
import org.folio.consortia.repository.PublicationTenantRequestRepository;
import org.folio.consortia.repository.SharingInstanceRepository;
import org.folio.consortia.repository.SharingPolicyRepository;
import org.folio.consortia.repository.SharingRoleRepository;
import org.folio.consortia.repository.SharingSettingRepository;
import org.folio.consortia.service.CleanupService;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.spring.FolioExecutionContext;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@Log4j2
@RequiredArgsConstructor
public class CleanupServiceImpl implements CleanupService {
  private final FolioExecutionContext folioExecutionContext;
  private final ConsortiumRepository consortiumRepository;
  private final PublicationStatusRepository publicationStatusRepository;
  private final PublicationTenantRequestRepository publicationTenantRequestRepository;
  private final SharingInstanceRepository sharingInstanceRepository;
  private final SharingRoleRepository sharingRoleRepository;
  private final SharingPolicyRepository sharingPolicyRepository;
  private final SharingSettingRepository sharingSettingRepository;

  @Value("${folio.timer.publication-records-max-age-in-seconds:86400}")
  private int recordMaxAge;

  @Override
  @Transactional
  public void clearPublicationTables() {
    log.info("clearPublicationTables:: Cleaning up publication records for tenant: {} ", folioExecutionContext.getTenantId());
    if (CollectionUtils.isEmpty(consortiumRepository.findAll())) {
      log.debug("clearPublicationTables:: Tenant '{}' is not consortia central tenant. Nothing to delete", folioExecutionContext.getTenantId());
      return;
    }

    var beforeDate = LocalDateTime.now().minus(recordMaxAge , ChronoUnit.SECONDS);
    log.info("clearPublicationRecords:: Cleaning up publication records created before {} for tenant: {} ", beforeDate, folioExecutionContext.getTenantId());

    int tenantRequestQuantity = publicationTenantRequestRepository.deleteAllByCreatedDateBefore(beforeDate);
    if (tenantRequestQuantity > 0) {
      log.info("clearPublicationTables:: Successfully removed {} pc_tenant_request records from tenant '{}'", tenantRequestQuantity, folioExecutionContext.getTenantId());
    }

    int statusRecordsQuantity = publicationStatusRepository.deleteAllByCreatedDateBefore(beforeDate);
    if (statusRecordsQuantity > 0 ) {
      log.info("clearPublicationTables:: Successfully removed {} pc_state records from tenant '{}'", statusRecordsQuantity, folioExecutionContext.getTenantId());
    }
  }

  @Override
  @Transactional
  public void clearSharingTables(String tenantId) {
    log.info("clearSharingTables:: Cleaning up sharing records for tenant: {} ", tenantId);
    if (CollectionUtils.isEmpty(consortiumRepository.findAll())) {
      log.debug("clearSharingTables:: Tenant '{}' is not consortia central tenant. Nothing to delete", folioExecutionContext.getTenantId());
      return;
    }

    int sharingInstanceQuantity = sharingInstanceRepository.deleteInstancesForTenant(tenantId);
    log.info("clearSharingTables:: Successfully removed {} sharing instance records for tenant '{}'", sharingInstanceQuantity, tenantId);

    int sharingRoleQuantity = sharingRoleRepository.deleteRolesForTenant(tenantId);
    log.info("clearSharingTables:: Successfully removed {} sharing role records for tenant '{}'", sharingRoleQuantity, tenantId);

    int sharingPolicyQuantity = sharingPolicyRepository.deletePoliciesForTenant(tenantId);
    log.info("clearSharingTables:: Successfully removed {} sharing policy records for tenant '{}'", sharingPolicyQuantity, tenantId);

    int sharingSettingQuantity = sharingSettingRepository.deleteRolesForTenant(tenantId);
    log.info("clearSharingTables:: Successfully removed {} sharing setting records for tenant '{}'", sharingSettingQuantity, tenantId);
  }
}
