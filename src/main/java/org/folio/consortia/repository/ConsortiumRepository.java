package org.folio.consortia.repository;

import org.folio.consortia.domain.entity.ConsortiumEntity;
import org.jetbrains.annotations.NotNull;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface ConsortiumRepository extends JpaRepository<ConsortiumEntity, UUID> {
  boolean existsById(@NotNull UUID consortiumId);
}
