package org.folio.consortia.repository;

import java.util.Set;
import java.util.UUID;

import org.folio.consortia.domain.entity.SharingRoleCapabilitySetEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

public interface SharingRoleCapabilitySetRepository extends JpaRepository<SharingRoleCapabilitySetEntity, UUID> {

  @Query("SELECT sr.tenantId FROM SharingRoleCapabilitySetEntity sr WHERE sr.roleId = ?1")
  Set<String> findTenantsByRoleId(UUID roleId);

  boolean existsByRoleId(UUID roleId);

  @Modifying
  @Query("DELETE FROM SharingRoleEntity sr WHERE sr.roleId = ?1")
  void deleteByRoleId(UUID roleId);
}
