package org.folio.consortia.service.impl;

import java.time.Instant;
import java.util.HashMap;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.folio.consortia.client.KeycloakClient;
import org.folio.consortia.config.keycloak.KeycloakLoginClientProperties;
import org.folio.consortia.config.keycloak.KeycloakProperties;
import org.folio.consortia.domain.dto.KeycloakClientCredentials;
import org.folio.consortia.service.KeycloakCredentialsService;
import org.folio.tools.store.SecureStore;
import org.folio.tools.store.exception.NotFoundException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class KeycloakCredentialsServiceImpl implements KeycloakCredentialsService {

  private static final String MASTER_REALM = "master";
  private static final String MASTER_TOKEN_CACHE_KEY = "'keycloak-admin-cli-token'";

  private final KeycloakClient keycloakClient;
  private final KeycloakProperties keycloakProperties;
  private final KeycloakLoginClientProperties keycloakClientProperties;
  private final SecureStore secureStore;
  private final TaskScheduler asyncTaskScheduler;

  @Value("${folio.environment}")
  private String folioEnvironment;

  @Cacheable(cacheNames = "keycloak-credentials", key = "{#centralTenant, #memberTenant}")
  public KeycloakClientCredentials getClientCredentials(String centralTenant, String memberTenant) {
    var clientId = memberTenant + keycloakClientProperties.getClientNameSuffix();
    var clientCredentials = keycloakClient.getClientCredentials(memberTenant, clientId, getMasterAuthToken());
    if (CollectionUtils.isEmpty(clientCredentials) || BooleanUtils.isNotTrue(clientCredentials.get(0).getEnabled())) {
      log.error("getClientCredentials:: Failed to get client credentials for tenant: {}", memberTenant);
      throw new IllegalStateException("Failed to get client credentials for tenant: %s".formatted(memberTenant));
    }
    return clientCredentials.get(0);
  }

  @Cacheable(cacheNames = "keycloak-token", key = MASTER_TOKEN_CACHE_KEY)
  public String getMasterAuthToken() {
    var clientId = keycloakProperties.getClientId();
    var clientSecret = retrieveKcClientSecret(MASTER_REALM, clientId);

    HashMap<String, String> loginRequest = new HashMap<>();
    loginRequest.put("client_id", clientId);
    loginRequest.put("client_secret", clientSecret);
    loginRequest.put("grant_type", keycloakProperties.getGrantType());

    log.info("requestToken:: Issuing access token for Keycloak communication [clientId: {}]", clientId);
    var token = keycloakClient.login(loginRequest);

    if (token.getExpiresIn() != null) {
      // Evict token 60 seconds before expiration time
      var expirationTime = Instant.now().plusSeconds(token.getExpiresIn()).minusSeconds(60);
      asyncTaskScheduler.schedule(this::evictMasterAuthToken, expirationTime);
    }

    return token.getTokenType() + " " + token.getAccessToken();
  }

  @CacheEvict(cacheNames = "keycloak-token", key = MASTER_TOKEN_CACHE_KEY)
  public void evictMasterAuthToken() {
    log.info("evictMasterAuthToken:: Evicting master realm admin token from cache");
  }

  private String retrieveKcClientSecret(String realm, String clientId) {
    if (Boolean.TRUE.equals(keycloakClientProperties.getSecureStoreDisabled())) {
      log.info("retrieveKcClientSecret:: Secure store is disabled. Using default client secret for clientId: {}", clientId);
      return "SecretPassword";
    }
    try {
      log.info("retrieveKcClientSecret:: Retrieving client secret from secure store");
      return secureStore.get("%s_%s_%s".formatted(folioEnvironment, realm, clientId));
    } catch (NotFoundException e) {
      log.error("retrieveKcClientSecret:: Client secret not found in secure store for clientId: {}", clientId);
      throw new IllegalStateException("Failed to get value from secure store [clientId: %s]".formatted(clientId), e);
    }
  }

}
