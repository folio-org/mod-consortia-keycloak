package org.folio.consortia.repository;

import java.util.List;
import java.util.Set;
import java.util.UUID;

import org.folio.consortia.domain.entity.SharingRoleEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

public interface SharingRoleRepository extends JpaRepository<SharingRoleEntity, UUID> {

  List<SharingRoleEntity> findByRoleId(UUID roleId);

  SharingRoleEntity findByRoleIdAndTenantId(UUID roleId, String tenantId);

  @Query("SELECT sr.tenantId FROM SharingRoleEntity sr WHERE sr.roleId = ?1")
  Set<String> findTenantsByRoleId(UUID roleId);

  Set<String> findTenantsByRoleIdAndIsCapabilitySetsSharedTrue(UUID roleId);

  Set<String> findTenantsByRoleIdAndIsCapabilitiesSharedTrue(UUID roleId);

  boolean existsByRoleId(UUID roleId);

  @Modifying
  void deleteByRoleId(UUID roleId);
}
