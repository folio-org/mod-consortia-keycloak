package org.folio.consortia.domain.entity;

import java.util.UUID;

import jakarta.persistence.Entity;
import jakarta.persistence.Table;

@Entity
@Table(name = "inactive_user_tenant")
public class InactiveUserTenantEntity extends UserTenantEntity {
  // Empty entity that simply maps to a different table
  // Inherits all fields and behavior from UserTenantEntity

  public static InactiveUserTenantEntity from(UserTenantEntity userTenantEntity) {
    return (InactiveUserTenantEntity) new InactiveUserTenantEntity()
      .setId(UUID.randomUUID())
      .setUserId(userTenantEntity.getUserId())
      .setUsername(userTenantEntity.getUsername())
      .setTenant(userTenantEntity.getTenant())
      .setIsPrimary(userTenantEntity.getIsPrimary());
  }
}

