package org.folio.consortia.service;

import org.folio.consortia.exception.ResourceNotFoundException;
import java.util.List;
import java.util.UUID;

import org.folio.consortia.domain.dto.Tenant;
import org.folio.consortia.domain.dto.TenantCollection;
import org.folio.consortia.domain.dto.TenantDetails;
import org.folio.consortia.domain.dto.TenantDetails.SetupStatusEnum;
import org.folio.consortia.domain.entity.TenantEntity;

public interface TenantService {

  /**
   * Gets tenant collection based on consortiumId.
   *
   * @param consortiumId  the consortiumId
   * @param limit  the limit
   * @param offset the offset
   * @return tenant collection
   */
  TenantCollection get(UUID consortiumId, Integer offset, Integer limit);

  /**
   * Gets all tenant collection based on consortiumId.
   *
   * @return tenant collection
   */
  TenantCollection getAll(UUID consortiumId);

  /**
   * Inserts single tenant based on consortiumId.
   *
   * @param consortiumId  the consortiumId
   * @param tenantDto  the tenantDto
   * @param adminUserId the id of admin_user
   * @return tenantDto
   */
  Tenant save(UUID consortiumId, UUID adminUserId, Tenant tenantDto);

  /**
   * Updates single tenant based on consortiumId.
   *
   * @param consortiumId  the consortiumId
   * @param tenantId the tenantId
   * @param tenantDto  the tenantDto
   * @return tenantDto
   */
  Tenant update(UUID consortiumId, String tenantId, Tenant tenantDto);

  /**
   * Updates tenant's setup status.
   *
   * @param tenantId the tenantId
   * @param centralTenantId  the consortiumId
   * @param setupStatus  the setup status
   */
  void updateTenantSetupStatus(String tenantId, String centralTenantId, SetupStatusEnum setupStatus);

  /**
   * Deletes single tenant based on consortiumId.
   * @param consortiumId the consortiumId
   * @param tenantId the tenantId
   */
  void delete(UUID consortiumId, String tenantId);

  /**
   * Gets tenant entity based on tenantId.
   *
   * @param tenantId the tenantId
   * @return tenant Entity
   */
  TenantEntity getByTenantId(String tenantId);

  /**
   * Gets tenant details based on tenantId.
   *
   * @param consortiumId the consortiumId
   * @param tenantId the tenantId
   * @return tenant details
   */
  TenantDetails getTenantDetailsById(UUID consortiumId, String tenantId);

  /**
   * Gets central tenant id from db
   * @return central tenant id
   */
  String getCentralTenantId();

  /**
   * Check for tenant existence in consortia
   * @throws ResourceNotFoundException in case if tenants absence in consortia
   */
  void checkTenantsAndConsortiumExistsOrThrow(UUID consortiumId, List<String> tenantIds);

  /**
   * Check whether tenant exists or throw ResourceNotFound exception
   */
  void checkTenantExistsOrThrow(String tenantId);
}
