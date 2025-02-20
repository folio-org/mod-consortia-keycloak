package org.folio.consortia.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.common.configuration.properties.FolioEnvironment;
import org.folio.consortia.config.keycloak.KeycloakProperties;
import org.folio.consortia.domain.dto.KeycloakRealmConfiguration;
import org.folio.tools.store.exception.NotFoundException;
import org.folio.tools.store.SecureStore;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Log4j2
public class RealmConfigurationProvider {

  private static final String MASTER_REALM = "master";
  private static final Boolean DISABLE_SECURE_STORE = Boolean.valueOf(System.getenv()
    .getOrDefault("DISABLE_SECURE_STORE", "false"));
  private final FolioEnvironment folioEnvironment;
  private final SecureStore secureStore;
  private final KeycloakProperties keycloakConfigurationProperties;

  /**
   * Provides realm configuration using {@link org.folio.spring.FolioExecutionContext} object.
   *
   * @return {@link KeycloakRealmConfiguration} object for user authentication
   */
  @Cacheable(cacheNames = "keycloak-configuration", key = "'keycloak-config'")
  public KeycloakRealmConfiguration getRealmConfiguration() {
    var clientId = keycloakConfigurationProperties.getClientId();
    return new KeycloakRealmConfiguration()
      .clientId(clientId)
      .clientSecret(retrieveKcClientSecret(MASTER_REALM, clientId));
  }

  /**
   * Provides configuration for a client.
   *
   * @return {@link KeycloakRealmConfiguration} object
   */
  @Cacheable(cacheNames = "keycloak-client-configuration", key = "{#realm, #clientId}")
  public KeycloakRealmConfiguration getClientConfiguration(String realm, String clientId) {
    return new KeycloakRealmConfiguration()
      .clientId(clientId)
      .clientSecret(retrieveKcClientSecret(realm, clientId));
  }

  @CacheEvict(cacheNames = "keycloak-client-configuration", allEntries = true)
  public void evictAllClientConfigurations() {
  }

  private String retrieveKcClientSecret(String realm, String clientId) {
    if (Boolean.TRUE.equals(DISABLE_SECURE_STORE)) {
      log.info("Secure store is disabled. Using default secret password");
      return "SecretPassword";
    }
    try {
      log.info("Secure store is enabled. Trying to get value from secure store");
      return secureStore.get(buildKey(folioEnvironment.getEnvironment(), realm, clientId));
    } catch (NotFoundException e) {
      throw new IllegalStateException(String.format(
        "Failed to get value from secure store [clientId: %s]", clientId), e);
    }
  }

  private String buildKey(String env, String realm, String clientId) {
    return String.format("%s_%s_%s", env, realm, clientId);
  }
}
