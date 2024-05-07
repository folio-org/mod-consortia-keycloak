package org.folio.consortia.repository;

import java.util.ArrayList;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.folio.consortia.domain.dto.Status;
import org.folio.consortia.domain.entity.SharingInstanceEntity;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface SharingInstanceRepository extends JpaRepository<SharingInstanceEntity, UUID>, JpaSpecificationExecutor<SharingInstanceEntity> {

  @Query("SELECT si FROM SharingInstanceEntity si WHERE si.instanceId = ?1 AND si.sourceTenantId= ?2 AND si.targetTenantId= ?3")
  Optional<SharingInstanceEntity> findByInstanceAndTenantIds(UUID instanceIdentifier, String sourceTenantId, String targetTenantId);

  interface Specifications {
    static Specification<SharingInstanceEntity> constructSpecification(UUID instanceIdentifier, String sourceTenantId,
        String targetTenantId, Status status) {
      var list = new ArrayList<Specification<SharingInstanceEntity>>();
      if (Objects.nonNull(instanceIdentifier)) {
        list.add(by("instanceId", instanceIdentifier));
      }
      if (StringUtils.isNotEmpty(sourceTenantId)) {
        list.add(by("sourceTenantId", sourceTenantId));
      }
      if (StringUtils.isNotEmpty(targetTenantId)) {
        list.add(by("targetTenantId", targetTenantId));
      }
      if (Objects.nonNull(status)) {
        list.add(by("status", status));
      }

      return list.stream().reduce(Specification::and).orElse(null);
    }

    static Specification<SharingInstanceEntity> by(String fieldName, Object fieldValue) {
      return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get(fieldName), fieldValue);
    }
  }
}
