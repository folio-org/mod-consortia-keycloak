package org.folio.consortia.domain.entity;

import org.folio.consortia.domain.entity.base.AuditableEntity;
import java.util.Objects;
import java.util.UUID;

import jakarta.persistence.Id;
import jakarta.persistence.MappedSuperclass;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
@RequiredArgsConstructor
@MappedSuperclass
public abstract class AbstractTenantEntity extends AuditableEntity {
  @Id
  private String id;
  private String code;
  private String name;
  private UUID consortiumId;
  private Boolean isCentral;

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof AbstractTenantEntity that)) return false;
    return Objects.equals(id, that.id) && Objects.equals(code, that.code) && Objects.equals(name, that.name) && Objects.equals(consortiumId, that.consortiumId) && Objects.equals(isCentral, that.isCentral);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, code, name, consortiumId, isCentral);
  }
}
