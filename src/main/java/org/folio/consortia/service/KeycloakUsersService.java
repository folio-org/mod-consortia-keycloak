package org.folio.consortia.service;

public interface KeycloakUsersService {

  /**
   * Create identity provider links for users
   *
   * @param centralTenantId central tenant id
   * @param memberTenantId id to of original tenant
   */
  void createUsersIdpLinks(String centralTenantId, String memberTenantId);

  /**
   * Remove identity provider links for users
   *
   * @param centralTenantId central tenant id
   * @param memberTenantId id to of original tenant
   */
  void removeUsersIdpLinks(String centralTenantId, String memberTenantId);

}
