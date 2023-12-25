package org.folio.consortia.domain.entity;

import org.folio.consortia.domain.entity.base.AuditableEntity;
import java.time.LocalDateTime;
import java.util.UUID;

import org.folio.consortia.domain.dto.PublicationStatus;
import org.springframework.data.annotation.LastModifiedDate;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
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
@Table(name = "pc_tenant_request")
public class PublicationTenantRequestEntity extends AuditableEntity {
  @Id
  private UUID id;

  @ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE })
  @JoinColumn(name = "pc_id", referencedColumnName = "id")
  private PublicationStatusEntity pcState;

  private String tenantId;

  @Enumerated(EnumType.STRING)
  private PublicationStatus status;

  private String requestUrl;
  private String requestPayload;
  private String response;
  private Integer responseStatusCode;

  @LastModifiedDate
  @Column(name = "completed_date")
  private LocalDateTime completedDate;

}
