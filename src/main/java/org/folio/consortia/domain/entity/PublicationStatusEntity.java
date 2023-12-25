package org.folio.consortia.domain.entity;

import org.folio.consortia.domain.entity.base.AuditableEntity;
import java.util.UUID;

import org.folio.consortia.domain.dto.PublicationStatus;

import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
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
@Table(name = "pc_state")
public class PublicationStatusEntity extends AuditableEntity {
  @Id
  private UUID id;

  @Enumerated(EnumType.STRING)
  private PublicationStatus status;

  private Integer totalRecords;
}
