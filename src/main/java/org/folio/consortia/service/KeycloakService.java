package org.folio.consortia.service;

public interface KeycloakService {

  /**
   * Creates an identity provider in the central tenant realm for the member tenant.
   * In case the identity provider already exists, it will not be created again to
   * support tenant removal and re-creation.
   *
   * @param centralTenant central tenant
   * @param memberTenant  member tenant
   */
  void createIdentityProvider(String centralTenant, String memberTenant);

  /**
   * Deletes an identity provider in the central tenant realm corresponding to the member tenant.
   *
   * @param centralTenant central tenant
   * @param memberTenant  member tenant
   */
  void deleteIdentityProvider(String centralTenant, String memberTenant);

}
