package org.folio.consortia.service;

public interface KeycloakService {

  /**
   * Adds a custom authentication flow for a central tenant.
   * <p>
   * This method performs the following steps:
   * 1. Duplicates the built-in browser authentication flow.
   * 2. Adds a custom ECS Folio authentication form provider to the duplicated flow.
   * 3. Fetches executions from the current flow.
   * 4. Deletes the default auth-username-password-form execution from the flow.
   * 5. Raises the priority of the custom ECS Folio authentication form provider.
   * 6. Binds the custom flow to the realm.
   *
   * @param centralTenantId the tenant id for which the custom authentication flow is to be added
   * @throws IllegalStateException if the required executions are not found
   */
  void addCustomAuthFlowForCentralTenant(String centralTenantId);

  /**
   * Creates an identity provider in the central tenant realm for the member tenant with conditions:<br/>
   * 1) In case the identity provider already exists, it will not be created again to
   * support tenant removal and re-creation.<br/>
   * 2) If the identity provider creation is disabled, the method will return without any action
   *
   * @param centralTenantId central tenant
   * @param memberTenantId  member tenant
   */
  void createIdentityProvider(String centralTenantId, String memberTenantId);

  /**
   * Deletes an identity provider in the central tenant realm corresponding to the member tenant.<br/>
   * If the identity provider creation is disabled, the method will return without any action
   *
   * @param centralTenantId central tenant
   * @param memberTenantId  member tenant
   */
  void deleteIdentityProvider(String centralTenantId, String memberTenantId);
}
