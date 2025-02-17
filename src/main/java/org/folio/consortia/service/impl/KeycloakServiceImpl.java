package org.folio.consortia.service.impl;

import static org.folio.consortia.utils.KeycloakUtils.buildIdpClientConfig;
import static org.folio.consortia.utils.KeycloakUtils.formatTenantField;

import org.folio.common.configuration.properties.FolioEnvironment;
import org.folio.consortia.client.KeycloakClient;
import org.folio.consortia.config.keycloak.KeycloakIdentityProviderProperties;
import org.folio.consortia.config.keycloak.KeycloakLoginClientProperties;
import org.folio.consortia.config.keycloak.KeycloakProperties;
import org.folio.consortia.domain.dto.KeycloakIdentityProvider;
import org.folio.consortia.service.KeycloakService;
import org.folio.tools.store.SecureStore;
import org.folio.tools.store.exception.NotFoundException;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import lombok.val;

@Service
@RequiredArgsConstructor
@Log4j2
public class KeycloakServiceImpl implements KeycloakService {

  private final KeycloakClient keycloakClient;
  private final KeycloakProperties keycloakProperties;
  private final KeycloakLoginClientProperties keycloakClientProperties;
  private final KeycloakIdentityProviderProperties keycloakIdpProperties;
  private final FolioEnvironment folioEnvironment;
  private final SecureStore secureStore;

  @Override
  public void createIdentityProvider(String centralTenant, String memberTenant) {
    if (!Boolean.TRUE.equals(keycloakIdpProperties.getIsEnabled())) {
      log.info("createIdentityProvider:: Identity provider creation is disabled. Skipping creation for tenant {}", memberTenant);
      return;
    }
    log.info("createIdentityProvider:: Creating identity provider for tenant {} in central realm {}", memberTenant, centralTenant);
    var providerAlias = formatTenantField(keycloakIdpProperties.getAlias(), memberTenant);

    if (keycloakClient.getIdentityProvider(centralTenant, providerAlias) != null) {
      log.info("createIdentityProvider:: Identity provider {} already exists for tenant {} in central realm {}", providerAlias, memberTenant, centralTenant);
      return;
    }

    var providerDisplayName = formatTenantField(keycloakIdpProperties.getDisplayName(), memberTenant);
    var clientId = memberTenant + keycloakClientProperties.getClientNameSuffix();
    var clientSecret = retrieveKcClientSecret(centralTenant, clientId);
    var clientConfig = buildIdpClientConfig(keycloakProperties.getUrl(), memberTenant, clientId, clientSecret);
    val idp = KeycloakIdentityProvider.builder()
      .alias(providerAlias)
      .displayName(providerDisplayName)
      .enabled(keycloakIdpProperties.getIsEnabled())
      .config(clientConfig)
      .build();

    keycloakClient.createIdentityProvider(centralTenant, idp);
  }

  @Override
  public void deleteIdentityProvider(String centralTenant, String memberTenant) {
    if (!isIdpCreationEnabled()) {
      log.info("deleteIdentityProvider:: Identity provider creation is disabled. Skipping deletion for tenant {}", memberTenant);
      return;
    }
    log.info("deleteIdentityProvider:: Deleting identity provider for realm {}", memberTenant);
    var providerAlias = formatTenantField(keycloakIdpProperties.getAlias(), memberTenant);
    keycloakClient.deleteIdentityProvider(centralTenant, providerAlias);
  }

  private String retrieveKcClientSecret(String realm, String clientId) {
    if (Boolean.TRUE.equals(keycloakClientProperties.getSecureStoreDisabled())) {
      log.info("retrieveKcClientSecret:: Secure store is disabled. Using default client secret");
      return "SecretPassword";
    }
    try {
      log.info("retrieveKcClientSecret:: Retrieving client secret from secure store");
      return secureStore.get("%s_%s_%s".formatted(folioEnvironment.getEnvironment(), realm, clientId));
    } catch (NotFoundException e) {
      throw new IllegalStateException(String.format(
        "Failed to get value from secure store [clientId: %s]", clientId), e);
    }
  }

  private boolean isIdpCreationEnabled() {
    return Boolean.TRUE.equals(keycloakIdpProperties.getIsEnabled());
  }

}
