package org.folio.consortia.service;

import org.folio.consortia.domain.dto.ConsortiaConfiguration;

public interface ConsortiaConfigurationService {
  /**
   * Get Config based on requested tenant.
   * If process failed, it will specify error with tenant id.
   * There will be only one record
   *
   * @param requestedTenantId id of tenant in folio execution context
   * @return central tenant id
   */
  String getCentralTenantId(String requestedTenantId);

  /**
   * Get Config based on tenant id which is fetched from header
   * @return ConsortiaConfiguration
   */
  ConsortiaConfiguration getConsortiaConfiguration();

  /**
   * Save new configuration with central tenant id as value.
   * This configuration will be stored in requested tenant schema
   *
   * @param centralTenantId id of central tenant for requested tenant
   */
  ConsortiaConfiguration createConfiguration(String centralTenantId);

}
