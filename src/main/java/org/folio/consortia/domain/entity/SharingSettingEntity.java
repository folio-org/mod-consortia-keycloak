package org.folio.consortia.domain.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.RequiredArgsConstructor;
import org.folio.consortia.domain.entity.base.AuditableEntity;

@Data
@EqualsAndHashCode(callSuper = false)
@Builder
@RequiredArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "sharing_setting")
public class SharingSettingEntity extends AuditableEntity {

  @Id
  private UUID id;
  private UUID settingId;
  private String tenantId;

}
