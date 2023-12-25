package org.folio.consortia.service.impl;

import org.folio.consortia.repository.ConsortiumRepository;
import org.folio.consortia.repository.PublicationStatusRepository;
import org.folio.consortia.repository.PublicationTenantRequestRepository;
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

  @Value("${folio.timer.publication-records-max-age-in-seconds:86400}")
  private int recordMaxAge;

  @Transactional
  public void clearPublicationTables() {
    if (CollectionUtils.isNotEmpty(consortiumRepository.findAll())) {
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
    else {
      log.debug("clearPublicationTables:: Tenant '{}' is not consortia central tenant. Nothing to delete", folioExecutionContext.getTenantId());
    }
  }
}
