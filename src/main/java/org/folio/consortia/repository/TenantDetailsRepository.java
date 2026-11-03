package org.folio.consortia.repository;

import org.folio.consortia.domain.dto.TenantDetails;
import org.folio.consortia.domain.entity.TenantDetailsEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

public interface TenantDetailsRepository extends JpaRepository<TenantDetailsEntity, String> {
  @Modifying
  @Transactional(propagation = Propagation.REQUIRES_NEW)
  @Query("UPDATE TenantDetailsEntity t SET t.setupStatus= ?1 WHERE t.id= ?2")
  int setSetupStatusByTenantId(TenantDetails.SetupStatusEnum setupStatus, String tenantId);
}
