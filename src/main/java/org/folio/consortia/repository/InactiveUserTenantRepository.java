package org.folio.consortia.repository;

import org.folio.consortia.domain.entity.InactiveUserTenantEntity;
import org.folio.consortia.domain.entity.UserTenantEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Repository
public interface InactiveUserTenantRepository extends JpaRepository<InactiveUserTenantEntity, UUID> {

  @Query("SELECT iut FROM InactiveUserTenantEntity iut WHERE iut.userId= ?1 AND iut.tenant.id= ?2")
  Optional<InactiveUserTenantEntity> findByUserIdAndTenantId(UUID userId, String tenantId);

  @Query("SELECT iut FROM InactiveUserTenantEntity iut WHERE iut.userId= ?1 AND iut.isPrimary= false")
  List<InactiveUserTenantEntity> getByUserIdAndIsPrimaryFalse(UUID userId);

  @Query("SELECT iut FROM InactiveUserTenantEntity iut WHERE iut.userId NOT IN (SELECT ut.userId FROM UserTenantEntity ut WHERE ut.userId= ?1 AND ut.isPrimary=true) AND iut.userId= ?1")
  List<InactiveUserTenantEntity> getOrphansByUserIdAndIsPrimaryFalse(UUID userId);

  @Modifying
  @Query("DELETE FROM InactiveUserTenantEntity iut WHERE iut.userId NOT IN (SELECT ut.userId FROM UserTenantEntity ut WHERE ut.userId= ?1 AND ut.isPrimary=true) AND iut.userId= ?1")
  void deleteOrphansByUserIdAndIsPrimaryFalse(UUID userId);

  @Modifying
  @Query("DELETE FROM InactiveUserTenantEntity iut WHERE iut.tenant.id= ?1")
  void deleteInactiveUserTenantsByTenantId(String id);

}

