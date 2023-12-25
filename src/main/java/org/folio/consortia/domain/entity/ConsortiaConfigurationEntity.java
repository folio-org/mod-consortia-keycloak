package org.folio.consortia.domain.entity;

import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import org.folio.consortia.domain.entity.base.AuditableEntity;

import java.util.Objects;
import java.util.UUID;

@Getter
@Setter
@ToString
@RequiredArgsConstructor
@Entity
@Table(name = "consortia_configuration")
public class ConsortiaConfigurationEntity extends AuditableEntity {
  @Id
  @GeneratedValue(strategy = GenerationType.UUID)
  private UUID id;
  private String centralTenantId;

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof ConsortiaConfigurationEntity that)) return false;
    return Objects.equals(id, that.id) && Objects.equals(centralTenantId, that.centralTenantId);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, centralTenantId);
  }
}
