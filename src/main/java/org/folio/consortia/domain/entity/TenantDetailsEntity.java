package org.folio.consortia.domain.entity;

import org.folio.consortia.domain.dto.TenantDetails;

import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
@Entity
@Table(name = "tenant")
public class TenantDetailsEntity extends AbstractTenantEntity {
  @Enumerated(EnumType.STRING)
  private TenantDetails.SetupStatusEnum setupStatus;
}
