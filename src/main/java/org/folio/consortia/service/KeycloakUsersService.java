package org.folio.consortia.service;

public interface KeycloakUsersService {

  /**
   * Create identity provider links for users
   *
   * @param tenantId id to of original tenant
   * @param centralTenantId central tenant id
   */
  void createUsersIdpLinks(String tenantId, String centralTenantId);

}
