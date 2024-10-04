package org.folio.consortia.repository;

import java.util.List;
import java.util.Set;
import java.util.UUID;

import org.folio.consortia.domain.entity.SharingRoleEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

public interface SharingRoleRepository extends JpaRepository<SharingRoleEntity, UUID> {

  List<SharingRoleEntity> findByRoleName(String roleName);

  SharingRoleEntity findByRoleNameAndTenantId(String roleName, String tenantId);

  @Query("SELECT sr.tenantId FROM SharingRoleEntity sr WHERE sr.roleName = ?1")
  Set<String> findTenantsByRoleName(String roleName);

  @Query("SELECT sr.tenantId FROM SharingRoleEntity sr WHERE sr.roleName = ?1 and sr.isCapabilitiesShared = true")
  Set<String> findTenantsByRoleNameAndIsCapabilitiesSharedTrue(String roleName);

  @Query("SELECT sr.tenantId FROM SharingRoleEntity sr WHERE sr.roleName = ?1 and sr.isCapabilitySetsShared = true")
  Set<String> findTenantsByRoleNameAndIsCapabilitySetsSharedTrue(String roleName);

  @Query("select s.roleId from SharingRoleEntity s where s.roleName = ?1 and s.tenantId = ?2")
  UUID findRoleIdByRoleNameAndTenantId(String roleName, String tenantId);

  boolean existsByRoleId(UUID roleId);

  boolean existsByRoleNameAndTenantId(String roleName, String tenantId);

  boolean existsByRoleNameAndTenantIdAndIsCapabilitiesSharedTrue(String roleName, String tenantId);

  @Modifying
  void deleteByRoleName(String roleName);
}
