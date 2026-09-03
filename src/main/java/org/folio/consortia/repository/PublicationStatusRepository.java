package org.folio.consortia.repository;

import java.time.LocalDateTime;
import java.util.UUID;

import org.folio.consortia.domain.entity.PublicationStatusEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface PublicationStatusRepository extends JpaRepository<PublicationStatusEntity, UUID> {

  @Modifying
  @Query("DELETE FROM PublicationStatusEntity ps WHERE ps.createdDate < ?1")
  Integer deleteAllByCreatedDateBefore(LocalDateTime yesterday);

}
