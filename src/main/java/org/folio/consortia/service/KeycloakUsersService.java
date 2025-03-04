package org.folio.consortia.service;

public interface KeycloakUsersService {

  /**
   * Migrate users to create identity provider links
   *
   * @param centralTenantId central tenant id
   */
  void migrateUsers(String centralTenantId);

}
