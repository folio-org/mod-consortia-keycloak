package org.folio.consortia.repository;

import org.folio.consortia.domain.entity.SharingPolicyEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.Set;
import java.util.UUID;

@Repository
public interface SharingPolicyRepository extends JpaRepository<SharingPolicyEntity, UUID> {

  @Query("SELECT sp.tenantId FROM SharingPolicyEntity sp WHERE sp.policyId = ?1")
  Set<String> findTenantsByPolicyId(UUID policyId);

  boolean existsByPolicyId(UUID policyId);

  boolean existsByPolicyIdAndTenantId(UUID policyId, String tenantId);

  @Modifying
  @Query("DELETE FROM SharingPolicyEntity sp WHERE sp.policyId = ?1")
  void deleteByPolicyId(UUID policyId);
}
