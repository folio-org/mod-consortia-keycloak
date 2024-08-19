package org.folio.consortia.domain.entity;

import java.util.Objects;
import java.util.UUID;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import org.folio.consortia.domain.entity.base.AuditableEntity;

@Getter
@Setter
@ToString
@RequiredArgsConstructor
@Entity
@Table(name = "sharing_role")
public class SharingRoleEntity extends AuditableEntity {
  @Id
  private UUID id;
  private UUID roleId;
  private String tenantId;

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    SharingRoleEntity that = (SharingRoleEntity) o;
    return Objects.equals(id, that.id)
      && Objects.equals(roleId, that.roleId)
      && Objects.equals(tenantId, that.tenantId);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, roleId, tenantId);
  }
}
