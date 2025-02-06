package org.folio.consortia.service;

import org.folio.consortia.domain.dto.TenantDeleteRequest;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.exception.ResourceAlreadyExistException;
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
   * Inserts single tenant
   *
   * @param tenantEntity the tenant entity
   * @return tenantDto
   */
  Tenant saveTenant(TenantEntity tenantEntity);

  /**
   * Inserts single tenant based on consortiumId
   *
   * @param consortiumId  the consortiumId
   * @param tenantDto  the tenantDto
   * @return tenantDto
   */
  Tenant saveTenant(UUID consortiumId, Tenant tenantDto);

  /**
   * Inserts single tenant based on consortiumId and setup status
   *
   * @param consortiumId  the consortiumId
   * @param tenantDto  the tenantDto
   * @param setupStatus  the setup status
   * @return tenantDto
   */
  Tenant saveTenantDetails(UUID consortiumId, Tenant tenantDto, TenantDetails.SetupStatusEnum setupStatus);

  /**
   * Inserts single user tenant based on consortiumId
   *
   * @param consortiumId  the consortiumId
   * @param user  the user
   * @param tenantDto  the tenantDto
   */
  void saveUserTenant(UUID consortiumId, User user, Tenant tenantDto);

  /**
   * Updates tenant's setup status.
   *
   * @param tenantId the tenantId
   * @param centralTenantId  the consortiumId
   * @param setupStatus  the setup status
   */
  void updateTenantSetupStatus(String tenantId, String centralTenantId, SetupStatusEnum setupStatus);

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
   * Checks if central tenant exists
   * @return true if central tenant exists, false otherwise
   */
  boolean centralTenantExists();

  /**
   * Checks if tenant with given name exists
   * @throws ResourceAlreadyExistException in case if tenant with given name or code already exists
   */
  void checkTenantUniqueNameAndCodeOrThrow(Tenant tenant);

  /**
   * Check for tenant existence in consortia
   * @throws ResourceNotFoundException in case if tenants absence in consortia
   */
  void checkTenantsAndConsortiumExistsOrThrow(UUID consortiumId, List<String> tenantIds);

  /**
   * Check whether tenant exists or throw ResourceNotFound exception
   */
  void checkTenantExistsOrThrow(String tenantId);

  /**
   * Deletes single tenant based on deleteType.
   * If deleteType is HARD, tenant will be deleted with all related data permanently.
   */
  void deleteTenant(TenantEntity tenant, TenantDeleteRequest.DeleteTypeEnum deleteType);
}
