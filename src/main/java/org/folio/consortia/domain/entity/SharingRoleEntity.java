package org.folio.consortia.domain.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.folio.consortia.domain.entity.base.AuditableEntity;

import java.util.UUID;

@Data
@EqualsAndHashCode(callSuper = false)
@Entity
@Table(name = "sharing_role")
public class SharingRoleEntity extends AuditableEntity {
  @Id
  private UUID id;
  private UUID roleId;
  private String tenantId;
  private Boolean isCapabilitySetsShared;
  private Boolean isCapabilitiesShared;
}
