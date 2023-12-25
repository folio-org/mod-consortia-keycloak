package org.folio.consortia.repository;

import org.folio.consortia.domain.dto.TenantDetails;
import org.folio.consortia.domain.entity.TenantDetailsEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

public interface TenantDetailsRepository extends JpaRepository<TenantDetailsEntity, String> {
  @Modifying
  @Query("UPDATE TenantDetailsEntity t SET t.setupStatus= ?1 WHERE t.id= ?2")
  void setSetupStatusByTenantId(TenantDetails.SetupStatusEnum setupStatus, String tenantId);
}
