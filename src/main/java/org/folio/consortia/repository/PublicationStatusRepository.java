package org.folio.consortia.repository;

import java.time.LocalDateTime;
import java.util.UUID;

import org.folio.consortia.domain.entity.PublicationStatusEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface PublicationStatusRepository extends JpaRepository<PublicationStatusEntity, UUID> {

  int deleteAllByCreatedDateBefore(LocalDateTime yesterday);

}
