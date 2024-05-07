package org.folio.consortia.repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.folio.consortia.domain.entity.TenantEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface TenantRepository extends JpaRepository<TenantEntity, String> {

  @Query("SELECT t FROM TenantEntity t WHERE t.consortiumId = ?1 and t.isDeleted = FALSE")
  Page<TenantEntity> findByConsortiumId(UUID consortiumId, Pageable pageable);

  @Query("SELECT t FROM TenantEntity t WHERE t.consortiumId = ?1 and t.isDeleted = FALSE")
  List<TenantEntity> findByConsortiumId(UUID consortiumId);

  @Query("SELECT t FROM TenantEntity t WHERE t.isCentral = true")
  Optional<TenantEntity> findCentralTenant();

  boolean existsByIsCentralTrue();
  @Query("SELECT CASE WHEN COUNT(t) > 0 THEN TRUE ELSE FALSE END FROM TenantEntity t " +
    "WHERE t.code = ?1 AND t.id != ?2")
  boolean existsByCodeForOtherTenant(String name, String tenantId);

  @Query("SELECT CASE WHEN COUNT(t) > 0 THEN TRUE ELSE FALSE END FROM TenantEntity t " +
    "WHERE t.name = ?1 AND t.id != ?2")
  boolean existsByNameForOtherTenant(String name, String tenantId);
}
