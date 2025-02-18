package org.folio.consortia.service.impl;

import static org.folio.consortia.utils.KeycloakUtils.buildIdpClientConfig;
import static org.folio.consortia.utils.KeycloakUtils.formatTenantField;

import org.apache.commons.lang3.BooleanUtils;
import org.folio.consortia.client.KeycloakClient;
import org.folio.consortia.config.keycloak.KeycloakIdentityProviderProperties;
import org.folio.consortia.config.keycloak.KeycloakProperties;
import org.folio.consortia.domain.dto.KeycloakIdentityProvider;
import org.folio.consortia.service.KeycloakCredentialsService;
import org.folio.consortia.service.KeycloakService;
import org.springframework.stereotype.Service;

import feign.FeignException;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import lombok.val;

@Service
@RequiredArgsConstructor
@Log4j2
public class KeycloakServiceImpl implements KeycloakService {

  private static final String KEYCLOAK_PROVIDER_ID = "keycloak-oidc";

  private final KeycloakClient keycloakClient;
  private final KeycloakProperties keycloakProperties;
  private final KeycloakIdentityProviderProperties keycloakIdpProperties;
  private final KeycloakCredentialsService keycloakCredentialsService;

  @Override
  public void createIdentityProvider(String centralTenant, String memberTenant) {
    if (isIdpCreationDisabled()) {
      log.info("createIdentityProvider:: Identity provider creation is disabled. Skipping creation for tenant {}", memberTenant);
      return;
    }
    log.info("createIdentityProvider:: Creating identity provider for tenant {} in central realm {}", memberTenant, centralTenant);
    var providerAlias = formatTenantField(keycloakIdpProperties.getAlias(), memberTenant);

    if (identityProviderExists(centralTenant, providerAlias)) {
      log.info("createIdentityProvider:: Identity provider {} already exists for tenant {} in central realm {}", providerAlias, memberTenant, centralTenant);
      return;
    }

    var providerDisplayName = formatTenantField(keycloakIdpProperties.getDisplayName(), memberTenant);
    var clientCredentials = keycloakCredentialsService.getClientCredentials(centralTenant, memberTenant);
    var clientConfig = buildIdpClientConfig(keycloakProperties.getUrl(), memberTenant, clientCredentials.clientId(), clientCredentials.clientSecret());
    val idp = KeycloakIdentityProvider.builder()
      .alias(providerAlias)
      .displayName(providerDisplayName)
      .providerId(KEYCLOAK_PROVIDER_ID)
      .config(clientConfig)
      .build();

    keycloakClient.createIdentityProvider(centralTenant, idp, getToken());
  }

  @Override
  public void deleteIdentityProvider(String centralTenant, String memberTenant) {
    if (isIdpCreationDisabled()) {
      log.info("deleteIdentityProvider:: Identity provider creation is disabled. Skipping deletion for tenant {}", memberTenant);
      return;
    }

    log.info("deleteIdentityProvider:: Deleting identity provider for realm {}", memberTenant);
    var providerAlias = formatTenantField(keycloakIdpProperties.getAlias(), memberTenant);

    if (!identityProviderExists(centralTenant, providerAlias)) {
      log.info("deleteIdentityProvider:: Identity provider {} does not exist for tenant {} in central realm {}", providerAlias, memberTenant, centralTenant);
      return;
    }

    keycloakClient.deleteIdentityProvider(centralTenant, providerAlias, getToken());
  }

  private boolean identityProviderExists(String realm, String providerAlias) {
    try {
      keycloakClient.getIdentityProvider(realm, providerAlias, getToken());
      return true;
    } catch (FeignException.NotFound ignored) {
      return false;
    }
  }

  private boolean isIdpCreationDisabled() {
    return BooleanUtils.isNotTrue(keycloakIdpProperties.getEnabled());
  }

  private String getToken() {
    return keycloakCredentialsService.getMasterAuthToken();
  }

}
