package org.folio.consortia.service;

public interface KeycloakUsersService {

  /**
   * Migrate users to create identity provider links
   *
   * @param tenantId tenant id
   */
  void migrateUsers(String tenantId);

}
