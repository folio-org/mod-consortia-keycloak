package org.folio.consortia.service.impl;

import static org.folio.consortia.utils.KeycloakUtils.buildIdpClientConfig;

import java.util.Map;

import com.fasterxml.jackson.databind.node.ObjectNode;

import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.consortia.client.KeycloakClient;
import org.folio.consortia.config.keycloak.KeycloakIdentityProviderProperties;
import org.folio.consortia.domain.dto.KeycloakIdentityProvider;
import org.folio.consortia.domain.dto.Tenant;
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

  private static final String CUSTOM_BROWSER_FLOW = "custom-browser";
  private static final String ECS_FOLIO_AUTH_USRNM_PWD_FORM = "ecs-folio-auth-usrnm-pwd-form";
  private static final String AUTH_USERNAME_PASSWORD_FORM = "auth-username-password-form";
  private static final String CUSTOM_BROWSER_FLOW_FORMS = CUSTOM_BROWSER_FLOW + "%20forms";
  private static final String KEYCLOAK_PROVIDER_ID = "keycloak-oidc";

  private final KeycloakClient keycloakClient;
  private final KeycloakIdentityProviderProperties keycloakIdpProperties;
  private final KeycloakCredentialsService keycloakCredentialsService;

  /**
   * Adds a custom authentication flow for a central tenant.
   * <p>
   * This method performs the following steps:
   * 1. Duplicates the built-in browser authentication flow.
   * 2. Adds a custom ECS Folio authentication form provider to the duplicated flow.
   * 3. Fetches executions from the current flow.
   * 4. Deletes the default auth-username-password-form execution from the flow.
   * 5. Raises the priority of the custom ECS Folio authentication form provider.
   * 6. Binds the custom flow to the realm.
   *
   * @param tenant the tenant for which the custom authentication flow is to be added
   * @throws IllegalStateException if the required executions are not found
   */
  @Override
  public void addCustomAuthFlowForCentralTenant(Tenant tenant) {
    log.debug("Trying to add custom authentication flow for tenant with id={}", tenant.getId());
    if (Boolean.FALSE.equals(tenant.getIsCentral())) {
      log.info("Tenant with id={} is not central, skipping custom authentication flow addition", tenant.getId());
      return;
    }

    var token = getToken();
    var tenantId = tenant.getId();

    // 1. Duplicate built-in browser authentication flow
    var browserFlowCopyConfig = Map.of("newName", CUSTOM_BROWSER_FLOW);
    keycloakClient.copyBrowserFlow(tenantId, browserFlowCopyConfig, token);

    // 2. Add custom ecs folio authentication form provider to the duplicated flow
    var browserFlowProviderConfig = Map.of("provider", ECS_FOLIO_AUTH_USRNM_PWD_FORM);
    keycloakClient.executeBrowserFlow(tenantId, CUSTOM_BROWSER_FLOW_FORMS, browserFlowProviderConfig, token);

    // 3. Fetch executions from current flow
    var executions = keycloakClient.getExecutions(tenantId, CUSTOM_BROWSER_FLOW, token);
    var authUsernamePasswordFormExecution = executions.stream()
      .filter(execution -> StringUtils.equals(execution.getProviderId(), AUTH_USERNAME_PASSWORD_FORM))
      .findFirst()
      .orElseThrow(() -> new IllegalStateException("auth-username-password-form execution not found"));
    var ecsFolioAuthUsernamePasswordFormExecution = executions.stream()
      .filter(execution -> StringUtils.equals(execution.getProviderId(), ECS_FOLIO_AUTH_USRNM_PWD_FORM))
      .findFirst()
      .orElseThrow(() -> new IllegalStateException("ecs-folio-auth-usrnm-pwd-form execution not found"));

    // 4. Delete default auth-username-password-form execution from the flow
    keycloakClient.deleteExecution(tenant.getId(), authUsernamePasswordFormExecution.getId(), token);

    // 5. Raise priority of the custom ecs folio authentication form provider
    keycloakClient.raisePriority(tenantId, ecsFolioAuthUsernamePasswordFormExecution.getId(), token);

    // 6. Bind the custom flow to the realm
    ObjectNode realm = keycloakClient.getRealm(tenantId, token);
    realm.put("browserFlow", CUSTOM_BROWSER_FLOW);
    keycloakClient.updateRealm(tenantId, realm, token);
    log.info("Custom authentication flow successfully added for tenant with id={}", tenantId);
  }

  @Override
  public void createIdentityProvider(String centralTenantId, String memberTenantId) {
    if (isIdpCreationDisabled()) {
      log.info("createIdentityProvider:: Identity provider creation is disabled. Skipping creation for tenant {}", memberTenantId);
      return;
    }
    log.info("createIdentityProvider:: Creating identity provider for tenant {} in central realm {}", memberTenantId, centralTenantId);
    var providerAlias = memberTenantId + keycloakIdpProperties.getAlias();
    var authToken = keycloakCredentialsService.getMasterAuthToken();

    if (identityProviderExists(centralTenantId, providerAlias, authToken)) {
      log.info("createIdentityProvider:: Identity provider {} already exists for tenant {} in central realm {}", providerAlias, memberTenantId, centralTenantId);
      return;
    }

    var clientCredentials = keycloakCredentialsService.getClientCredentials(memberTenantId, authToken);
    var clientConfig = buildIdpClientConfig(keycloakIdpProperties.getBaseUrl(), memberTenantId, clientCredentials.getClientId(), clientCredentials.getSecret());

    var providerDisplayName = StringUtils.capitalize(memberTenantId) + " " + keycloakIdpProperties.getDisplayName();
    val idp = KeycloakIdentityProvider.builder()
      .alias(providerAlias)
      .displayName(providerDisplayName)
      .providerId(KEYCLOAK_PROVIDER_ID)
      .config(clientConfig)
      .build();

    keycloakClient.createIdentityProvider(centralTenantId, idp, authToken);
  }

  @Override
  public void deleteIdentityProvider(String centralTenantId, String memberTenantId) {
    if (isIdpCreationDisabled()) {
      log.info("deleteIdentityProvider:: Identity provider creation is disabled. Skipping deletion for tenant {}", memberTenantId);
      return;
    }

    log.info("deleteIdentityProvider:: Deleting identity provider for realm {}", memberTenantId);
    var providerAlias = memberTenantId + keycloakIdpProperties.getAlias();
    var authToken = keycloakCredentialsService.getMasterAuthToken();

    if (!identityProviderExists(centralTenantId, providerAlias, authToken)) {
      log.info("deleteIdentityProvider:: Identity provider {} does not exist for tenant {} in central realm {}", providerAlias, memberTenantId, centralTenantId);
      return;
    }

    keycloakClient.deleteIdentityProvider(centralTenantId, providerAlias, authToken);
  }

  private boolean identityProviderExists(String realm, String providerAlias, String authToken) {
    try {
      keycloakClient.getIdentityProvider(realm, providerAlias, authToken);
      return true;
    } catch (FeignException.NotFound ignored) {
      return false;
    }
  }

  private boolean isIdpCreationDisabled() {
    return BooleanUtils.isNotTrue(keycloakIdpProperties.getEnabled());
  }
}
