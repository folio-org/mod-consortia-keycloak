package org.folio.consortia.service;

import java.util.UUID;

import org.folio.consortia.domain.dto.IdentityProviderCreateRequest;
import org.folio.consortia.domain.dto.Tenant;
import org.folio.consortia.domain.dto.TenantCollection;
import org.folio.consortia.domain.dto.TenantDeleteRequest;

public interface TenantManager {

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
   * Inserts single tenant based on consortiumId.
   * Method checks whether requesting tenant is soft deleted or new tenant.
   * For re-adding soft deleted tenant,
   *   tenant is_deleted flag will be changed to false and dummy user will be created in mod_users.user-tenants table
   * For new tenant, all necessary actions will be done.
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
   * Deletes single tenant based on consortiumId.
   * @param consortiumId the consortiumId
   * @param tenantId the tenantId
   * @param tenantDeleteRequest object with flag indicating whether tenant should be deleted permanently or soft deleted
   *                            with related delete options. Delete type is determined by the value of the flag:<br/>
   *                            - <code>true</code>: tenant should be deleted permanently with all related data<br/>
   *                            - <code>false</code>: tenant should be soft deleted
   */
  void delete(UUID consortiumId, String tenantId, TenantDeleteRequest tenantDeleteRequest);


  /**
   * Creates an identity provider for the member tenant in the execution context tenant realm.
   *
   * @param memberTenantId the tenant to create the identity provider for
   * @param idpCreateRequest the identity provider create request with required parameters
   */
  void createIdentityProvider(String memberTenantId, IdentityProviderCreateRequest idpCreateRequest);

  /**
   * Deletes an identity provider of the member tenant from the execution context tenant realm.
   *
   * @param memberTenantId the tenant to delete the identity provider of
   */
  void deleteIdentityProvider(String memberTenantId);

  /**
   * Sets up custom keycloak login flow for the central tenant to be able to authenticate users created in member tenants.
   *
   * @param consortiumId the consortium id
   * @param centralTenantId the central tenant id
   */
	void setupCustomLogin(UUID consortiumId, String centralTenantId);

}
