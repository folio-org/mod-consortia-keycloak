package org.folio.consortia.domain.entity;

import java.util.Objects;
import java.util.UUID;

import org.folio.consortia.domain.entity.base.AuditableEntity;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
@RequiredArgsConstructor
@Entity
@Table(name = "sharing_setting")
public class SharingSettingEntity extends AuditableEntity {
  @Id
  private UUID id;
  private UUID settingId;
  private String tenantId;

  @Override
  public boolean equals(Object o) {
    if (this == o)
      return true;
    if (!(o instanceof SharingSettingEntity that))
      return false;
    return Objects.equals(id, that.id)
      && Objects.equals(settingId, that.settingId)
      && Objects.equals(tenantId, that.tenantId);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, settingId, tenantId);
  }
}
