package org.folio.consortia.service.impl;

import java.util.HashMap;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.folio.consortia.client.KeycloakClient;
import org.folio.consortia.config.keycloak.KeycloakLoginClientProperties;
import org.folio.consortia.config.keycloak.KeycloakProperties;
import org.folio.consortia.domain.dto.KeycloakClientCredentials;
import org.folio.consortia.service.KeycloakCredentialsService;
import org.folio.tools.store.SecureStore;
import org.folio.tools.store.exception.SecretNotFoundException;
import org.folio.tools.store.properties.SecureStoreProperties;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class KeycloakCredentialsServiceImpl implements KeycloakCredentialsService {

  private static final String MASTER_REALM = "master";

  private final KeycloakClient keycloakClient;
  private final KeycloakProperties keycloakProperties;
  private final KeycloakLoginClientProperties keycloakClientProperties;
  private final SecureStore secureStore;
  private final SecureStoreProperties secureStoreProperties;

  public KeycloakClientCredentials getClientCredentials(String tenantId, String token) {
    var clientId = tenantId + keycloakClientProperties.getClientNameSuffix();
    var clientCredentials = keycloakClient.getClientCredentials(tenantId, clientId, token);
    if (CollectionUtils.isEmpty(clientCredentials) || BooleanUtils.isNotTrue(clientCredentials.get(0).getEnabled())) {
      log.error("getClientCredentials:: Failed to get client credentials for tenant: {}", tenantId);
      throw new IllegalStateException("Failed to get client credentials for tenant: %s".formatted(tenantId));
    }
    return clientCredentials.get(0);
  }

  public String getMasterAuthToken() {
    var clientId = keycloakProperties.getClientId();
    var clientSecret = getBackendAdminClientSecret(clientId);

    HashMap<String, String> loginRequest = new HashMap<>();
    loginRequest.put("client_id", clientId);
    loginRequest.put("client_secret", clientSecret);
    loginRequest.put("grant_type", keycloakProperties.getGrantType());

    log.info("requestToken:: Issuing access token for Keycloak communication [clientId: {}]", clientId);
    var token = keycloakClient.login(loginRequest);
    return token.getTokenType() + " " + token.getAccessToken();
  }

  private String getBackendAdminClientSecret(String clientId) {
    try {
      log.info("getBackendAdminClientSecret:: Retrieving backend admin client secret from secure store");
      return secureStore.get("%s_%s_%s".formatted(secureStoreProperties.getEnvironment(), MASTER_REALM, clientId));
    } catch (SecretNotFoundException e) {
      log.error("getBackendAdminClientSecret:: Backend admin client secret not found in secure store for clientId: {}", clientId);
      throw new IllegalStateException("Failed to get value from secure store [clientId: %s]".formatted(clientId), e);
    }
  }

}
