package org.folio.consortia.service.impl;

import static org.folio.consortia.utils.KeycloakUtils.buildIdpClientConfig;

import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.consortia.client.KeycloakClient;
import org.folio.consortia.config.keycloak.KeycloakIdentityProviderProperties;
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
  private final KeycloakIdentityProviderProperties keycloakIdpProperties;
  private final KeycloakCredentialsService keycloakCredentialsService;

  @Override
  public void createIdentityProvider(String centralTenantId, String memberTenantId) {
    if (isIdpCreationDisabled()) {
      log.info("createIdentityProvider:: Identity provider creation is disabled. Skipping creation for tenant {}", memberTenantId);
      return;
    }
    log.info("createIdentityProvider:: Creating identity provider for tenant {} in central realm {}", memberTenantId, centralTenantId);
    var providerAlias = memberTenantId + keycloakIdpProperties.getAlias();

    if (identityProviderExists(centralTenantId, providerAlias)) {
      log.info("createIdentityProvider:: Identity provider {} already exists for tenant {} in central realm {}", providerAlias, memberTenantId, centralTenantId);
      return;
    }

    var providerDisplayName = StringUtils.capitalize(memberTenantId) + " " + keycloakIdpProperties.getDisplayName();
    var clientCredentials = keycloakCredentialsService.getClientCredentials(memberTenantId, getToken());
    var clientConfig = buildIdpClientConfig(keycloakIdpProperties.getBaseUrl(), memberTenantId, clientCredentials.getClientId(), clientCredentials.getSecret());
    val idp = KeycloakIdentityProvider.builder()
      .alias(providerAlias)
      .displayName(providerDisplayName)
      .providerId(KEYCLOAK_PROVIDER_ID)
      .config(clientConfig)
      .build();

    keycloakClient.createIdentityProvider(centralTenantId, idp, getToken());
  }

  @Override
  public void deleteIdentityProvider(String centralTenantId, String memberTenantId) {
    if (isIdpCreationDisabled()) {
      log.info("deleteIdentityProvider:: Identity provider creation is disabled. Skipping deletion for tenant {}", memberTenantId);
      return;
    }

    log.info("deleteIdentityProvider:: Deleting identity provider for realm {}", memberTenantId);
    var providerAlias = memberTenantId + keycloakIdpProperties.getAlias();

    if (!identityProviderExists(centralTenantId, providerAlias)) {
      log.info("deleteIdentityProvider:: Identity provider {} does not exist for tenant {} in central realm {}", providerAlias, memberTenantId, centralTenantId);
      return;
    }

    keycloakClient.deleteIdentityProvider(centralTenantId, providerAlias, getToken());
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
