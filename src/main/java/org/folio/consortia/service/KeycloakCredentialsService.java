package org.folio.consortia.service;

import org.folio.consortia.domain.dto.KeycloakClientCredentials;

public interface KeycloakCredentialsService {

  /**
   * Returns client credentials for a login client of a tenant
   *
   * @param tenantId tenant to fetch client credentials for
   * @param token    authorization token for keycloak client
   * @return Credentials for a tenant login client
   */
  KeycloakClientCredentials getClientCredentials(String tenantId, String token) ;

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
