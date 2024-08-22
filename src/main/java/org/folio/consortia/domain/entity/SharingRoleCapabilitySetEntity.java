package org.folio.consortia.domain.entity;

import java.util.List;
import java.util.UUID;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.folio.consortia.domain.entity.base.AuditableEntity;

@Data
@EqualsAndHashCode(callSuper = false)
@Entity
@Table(name = "sharing_role")
public class SharingRoleCapabilitySetEntity extends AuditableEntity {

  @Id
  private UUID id;
  private UUID roleId;
  private List<UUID> capabilitySetIds;
  private String tenantId;

}
