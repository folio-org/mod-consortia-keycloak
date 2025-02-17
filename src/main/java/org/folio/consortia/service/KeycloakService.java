package org.folio.consortia.service;

public interface KeycloakService {

  /**
   * Creates an identity provider in the central tenant realm for the member tenant with conditions:<br/>
   * 1) In case the identity provider already exists, it will not be created again to
   * support tenant removal and re-creation.<br/>
   * 2) If the identity provider creation is disabled, the method will return without any action
   *
   * @param centralTenant central tenant
   * @param memberTenant  member tenant
   */
  void createIdentityProvider(String centralTenant, String memberTenant);

  /**
   * Deletes an identity provider in the central tenant realm corresponding to the member tenant.<br/>
   * If the identity provider creation is disabled, the method will return without any action
   *
   * @param centralTenant central tenant
   * @param memberTenant  member tenant
   */
  void deleteIdentityProvider(String centralTenant, String memberTenant);

}
