package org.folio.consortia.repository;

import java.time.LocalDateTime;
import java.util.UUID;

import org.folio.consortia.domain.entity.PublicationTenantRequestEntity;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface PublicationTenantRequestRepository extends JpaRepository<PublicationTenantRequestEntity, UUID> {

  Page<PublicationTenantRequestEntity> findByPcStateId(UUID publicationId, Pageable pageable);
  void deleteByPcStateId(UUID publicationId);

  int deleteAllByCreatedDateBefore(LocalDateTime yesterday);
}
