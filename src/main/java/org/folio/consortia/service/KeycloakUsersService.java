package org.folio.consortia.service;

public interface KeycloakUsersService {

  /**
   * Migrate users to create identity provider links
   *
   * @param tenantId tenant id to migrate users from
   * @param centralTenantId central tenant id to migrate users to
   */
  void migrateUsers(String tenantId, String centralTenantId);

}
