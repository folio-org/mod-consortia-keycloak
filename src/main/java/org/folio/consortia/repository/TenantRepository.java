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

  Page<TenantEntity> findByConsortiumId(UUID consortiumId, Pageable pageable);
  List<TenantEntity> findByConsortiumId(UUID consortiumId);

  @Query("SELECT t FROM TenantEntity t where t.isCentral = true")
  Optional<TenantEntity> findCentralTenant();

  boolean existsByIsCentralTrue();
  boolean existsByCode(String code);
  boolean existsByName(String name);
}
