package org.folio.consortia.service;

import org.folio.consortia.domain.dto.KeycloakClientCredentials;

public interface KeycloakCredentialsService {

  /**
   * Returns client id and secret for member tenant login client in central realm
   *
   * @param centralTenant central tenant for getting correct realm
   * @param memberTenant member tenant for getting correct client id
   * @return Credentials for member tenant client in central realm
   */
  KeycloakClientCredentials getClientCredentials(String centralTenant, String memberTenant) ;

  /**
   * Returns master realm admin token for Keycloak communication
   *
   * @return Master realm admin token
   */
  String getMasterAuthToken();

  /**
   * Evicts master realm admin token from cache
   */
  void evictMasterAuthToken();

}
