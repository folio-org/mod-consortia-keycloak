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

  /**
   * Recreate identity provider link for a user
   *
   * @param centralTenantId central tenant id
   * @param userId user id
   */
  void recreateUserIdpLink(String centralTenantId, String userId);

}
