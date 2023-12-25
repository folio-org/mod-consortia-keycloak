package org.folio.consortia.repository;

import org.folio.consortia.domain.entity.ConsortiaConfigurationEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface ConsortiaConfigurationRepository extends JpaRepository<ConsortiaConfigurationEntity, UUID> {
}
