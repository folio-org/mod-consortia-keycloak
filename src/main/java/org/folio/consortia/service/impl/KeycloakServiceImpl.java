package org.folio.consortia.service.impl;

import java.util.Map;

import com.fasterxml.jackson.databind.node.ObjectNode;

import lombok.extern.log4j.Log4j2;
import org.folio.consortia.client.KeycloakClient;
import org.folio.consortia.domain.dto.Tenant;
import org.folio.consortia.service.KeycloakService;
import org.folio.consortia.service.TokenService;
import org.springframework.stereotype.Service;

@Service
@Log4j2
public class KeycloakServiceImpl implements KeycloakService {
  private static final String CUSTOM_BROWSER_FLOW = "custom-browser";
  private static final String ECS_FOLIO_AUTH_USRNM_PWD_FORM = "ecs-folio-auth-usrnm-pwd-form";
  private static final String AUTH_USERNAME_PASSWORD_FORM = "auth-username-password-form";

  private final KeycloakClient keycloakClient;
  private final TokenService tokenService;

  public KeycloakServiceImpl(KeycloakClient keycloakClient, TokenService tokenService) {
    this.keycloakClient = keycloakClient;
    this.tokenService = tokenService;
  }

  @Override
  public void addCustomAuthFlowForCentralTenant(Tenant tenant) {
    log.debug("Trying to add custom authentication flow for tenant with id={}", tenant.getId());
    if (Boolean.FALSE.equals(tenant.getIsCentral())) {
      return;
    }

    var token = getToken();
    var tenantId = tenant.getId();

    // 1. Duplicate built-in browser authentication flow
    var browserFlowCopyConfig = Map.of("newName", CUSTOM_BROWSER_FLOW);
    keycloakClient.copyBrowserFlow(tenantId, browserFlowCopyConfig, token);

    // 2. Add custom ecs folio authentication form provider to the duplicated flow
    var browserFlowProviderConfig = Map.of("provider", ECS_FOLIO_AUTH_USRNM_PWD_FORM);
    keycloakClient.executeBrowserFlow(tenantId, CUSTOM_BROWSER_FLOW, browserFlowProviderConfig, token);

    // 3. Fetch executions from current flow
    var executions = keycloakClient.getExecutions(tenantId, CUSTOM_BROWSER_FLOW, token);
    var authUsernamePasswordFormExecution = executions.stream()
      .filter(execution -> execution.getProviderId().equals(AUTH_USERNAME_PASSWORD_FORM))
      .findFirst()
      .orElseThrow(() -> new IllegalStateException("auth-username-password-form execution not found"));
    var ecsFolioAuthUsernamePasswordFormExecution = executions.stream()
      .filter(execution -> execution.getProviderId().equals(ECS_FOLIO_AUTH_USRNM_PWD_FORM))
      .findFirst()
      .orElseThrow(() -> new IllegalStateException("ecs-folio-auth-usrnm-pwd-form execution not found"));

    // 4. Delete default auth-username-password-form execution from the flow
    keycloakClient.deleteExecution(tenant.getId(), CUSTOM_BROWSER_FLOW, authUsernamePasswordFormExecution.getId(), token);

    // 5. Raise priority of the custom ecs folio authentication form provider
    keycloakClient.raisePriority(tenantId, CUSTOM_BROWSER_FLOW, ecsFolioAuthUsernamePasswordFormExecution.getId(), token);

    // 6. Bind the custom flow to the realm
    ObjectNode realm = keycloakClient.getRealm(tenantId, token);
    realm.put("browserFlow", CUSTOM_BROWSER_FLOW);
    keycloakClient.updateRealm(tenantId, realm, token);
  }

  private String getToken() {
    return tokenService.issueToken();
  }
}
