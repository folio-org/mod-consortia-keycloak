package org.folio.consortia.service.impl;

import static org.folio.consortia.utils.KeycloakUtils.buildIdentityProviderConfig;
import static org.folio.consortia.utils.KeycloakUtils.formatTenantField;

import org.folio.consortia.client.KeycloakClient;
import org.folio.consortia.config.keycloak.KeycloakIdentityProviderProperties;
import org.folio.consortia.config.keycloak.KeycloakProperties;
import org.folio.consortia.domain.dto.KeycloakIdentityProvider;
import org.folio.consortia.service.KeycloakService;
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
  private final KeycloakIdentityProviderProperties keycloakIdpProperties;

  @Override
  public void createIdentityProvider(String centralTenant, String memberTenant, String clientId, String clientSecret) {
    log.info("createIdentityProvider:: Creating identity provider for tenant {} in central realm {}", memberTenant, centralTenant);
    var providerAlias = formatTenantField(keycloakIdpProperties.getAlias(), memberTenant);

    if (keycloakClient.getIdentityProvider(centralTenant, providerAlias) != null) {
      log.info("createIdentityProvider:: Identity provider {} already exists for tenant {} in central realm {}", providerAlias, memberTenant, centralTenant);
      return;
    }

    var providerDisplayName = formatTenantField(keycloakIdpProperties.getDisplayName(), memberTenant);
    var clientConfig = buildIdentityProviderConfig(keycloakProperties.getUrl(), memberTenant, clientId, clientSecret);
    val idp = KeycloakIdentityProvider.builder()
      .alias(providerAlias)
      .displayName(providerDisplayName)
      .enabled(keycloakIdpProperties.getIsEnabled())
      .config(clientConfig)
      .build();

    keycloakClient.createIdentityProvider(centralTenant, idp);
  }

  @Override
  public void deleteIdentityProvider(String centralRealm, String tenantRealm) {
    log.info("deleteIdentityProvider:: Deleting identity provider for realm {}", tenantRealm);
    var providerAlias = formatTenantField(keycloakIdpProperties.getAlias(), tenantRealm);
    keycloakClient.deleteIdentityProvider(centralRealm, providerAlias);
  }

}
