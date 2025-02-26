package org.folio.consortia.service;

import static org.folio.consortia.service.KeycloakCredentialsServiceTest.createClientCredentials;
import static org.folio.consortia.support.EntityUtils.CENTRAL_TENANT_ID;
import static org.folio.consortia.support.EntityUtils.TENANT_ID;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;

import feign.FeignException;
import feign.Request;
import org.folio.consortia.client.KeycloakClient;
import org.folio.consortia.config.keycloak.KeycloakIdentityProviderProperties;
import org.folio.consortia.config.keycloak.KeycloakLoginClientProperties;
import org.folio.consortia.domain.dto.KeycloakIdentityProvider;
import org.folio.consortia.domain.dto.RealmExecutions;
import org.folio.consortia.service.impl.KeycloakServiceImpl;
import org.folio.consortia.support.CopilotGenerated;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.mock.mockito.SpyBean;

@SpringBootTest
@CopilotGenerated(partiallyGenerated = true)
class KeycloakServiceTest {

  private static final String AUTH_TOKEN = "token";
  private static final String CLIENT_SECRET = "secret";

  @MockBean
  private KeycloakClient keycloakClient;
  @MockBean
  private KeycloakCredentialsService keycloakCredentialsService;

  @SpyBean
  private KeycloakIdentityProviderProperties keycloakIdpProperties;
  @SpyBean
  private KeycloakLoginClientProperties keycloakLoginClientProperties;

  @Autowired
  private KeycloakServiceImpl keycloakService;
  @Value("${folio.environment}")
  private String folioEnvironment;

  @Captor
  private ArgumentCaptor<Map<String, String>> mapCaptor;

  @BeforeEach
  void setUp() {
    when(keycloakIdpProperties.getEnabled()).thenReturn(true);
    when(keycloakCredentialsService.getMasterAuthToken()).thenReturn(AUTH_TOKEN);
  }

  @Test
  void createIdentityProvider_createsProviderSuccessfully() {
    var alias = getTenantClientAlias(TENANT_ID);
    var clientId = TENANT_ID + keycloakLoginClientProperties.getClientNameSuffix();
    when(keycloakClient.getIdentityProvider(CENTRAL_TENANT_ID, alias, AUTH_TOKEN))
      .thenThrow(new FeignException.NotFound("not found", mock(Request.class), new byte[0], null));
    when(keycloakCredentialsService.getClientCredentials(TENANT_ID, AUTH_TOKEN))
      .thenReturn(createClientCredentials(clientId, CLIENT_SECRET, true));

    keycloakService.createIdentityProvider(CENTRAL_TENANT_ID, TENANT_ID);

    verify(keycloakClient).getIdentityProvider(CENTRAL_TENANT_ID, alias, AUTH_TOKEN);
    verify(keycloakClient).createIdentityProvider(eq(CENTRAL_TENANT_ID), any(KeycloakIdentityProvider.class), eq(AUTH_TOKEN));
  }

  @Test
  void createIdentityProvider_skipsIfDisabled() {
    var alias = getTenantClientAlias(TENANT_ID);
    when(keycloakIdpProperties.getEnabled()).thenReturn(false);

    keycloakService.createIdentityProvider(CENTRAL_TENANT_ID, TENANT_ID);

    verify(keycloakClient, never()).getIdentityProvider(CENTRAL_TENANT_ID, alias, AUTH_TOKEN);
    verify(keycloakClient, never()).createIdentityProvider(anyString(), any(KeycloakIdentityProvider.class), eq(AUTH_TOKEN));
  }

  @Test
  void createIdentityProvider_skipsIfProviderExists() {
    var alias = getTenantClientAlias(TENANT_ID);
    when(keycloakClient.getIdentityProvider(CENTRAL_TENANT_ID, alias, AUTH_TOKEN)).thenReturn(new KeycloakIdentityProvider());

    keycloakService.createIdentityProvider(CENTRAL_TENANT_ID, TENANT_ID);

    verify(keycloakClient).getIdentityProvider(CENTRAL_TENANT_ID, alias, AUTH_TOKEN);
    verify(keycloakClient, never()).createIdentityProvider(anyString(), any(KeycloakIdentityProvider.class), eq(AUTH_TOKEN));
  }

  @Test
  void deleteIdentityProvider_deletesProviderSuccessfully() {
    keycloakService.deleteIdentityProvider(CENTRAL_TENANT_ID, TENANT_ID);

    verify(keycloakClient).getIdentityProvider(CENTRAL_TENANT_ID, getTenantClientAlias(TENANT_ID), AUTH_TOKEN);
    verify(keycloakClient).deleteIdentityProvider(CENTRAL_TENANT_ID, getTenantClientAlias(TENANT_ID), AUTH_TOKEN);
  }

  @Test
  void deleteIdentityProvider_skipsIfDisabled() {
    var alias = getTenantClientAlias(TENANT_ID);
    when(keycloakClient.getIdentityProvider(CENTRAL_TENANT_ID, alias, AUTH_TOKEN))
      .thenThrow(new FeignException.NotFound("not found", mock(Request.class), new byte[0], null));

    keycloakService.deleteIdentityProvider(CENTRAL_TENANT_ID, TENANT_ID);

    verify(keycloakClient).getIdentityProvider(anyString(), anyString(), eq(AUTH_TOKEN));
    verify(keycloakClient, never()).deleteIdentityProvider(anyString(), anyString(), eq(AUTH_TOKEN));
  }

  @Test
  void deleteIdentityProvider_skipsIfProviderDoesNotExist() {
    when(keycloakIdpProperties.getEnabled()).thenReturn(false);

    keycloakService.deleteIdentityProvider(CENTRAL_TENANT_ID, TENANT_ID);

    verify(keycloakClient, never()).deleteIdentityProvider(anyString(), anyString(), eq(AUTH_TOKEN));
    verify(keycloakClient, never()).getIdentityProvider(anyString(), anyString(), eq(AUTH_TOKEN));
  }

  @Test
  void addCustomAuthFlowForCentralTenantSuccess() {
    var executions = List.of(
      new RealmExecutions().withId("id1").withProviderId("auth-username-password-form"),
      new RealmExecutions().withId("id2").withProviderId("ecs-folio-auth-usrnm-pwd-form"));
    var realm = new ObjectNode(JsonNodeFactory.instance);

    when(keycloakCredentialsService.getMasterAuthToken()).thenReturn(AUTH_TOKEN);
    when(keycloakClient.getExecutions(anyString(), anyString(), anyString())).thenReturn(executions);
    when(keycloakClient.getRealm(anyString(), anyString())).thenReturn(realm);

    keycloakService.addCustomAuthFlowForCentralTenant(TENANT_ID);

    verify(keycloakClient).copyBrowserFlow(eq(TENANT_ID), mapCaptor.capture(), eq(AUTH_TOKEN));
    verify(keycloakClient).executeBrowserFlow(eq(TENANT_ID), eq("custom-browser%20forms"), mapCaptor.capture(), eq(AUTH_TOKEN));
    verify(keycloakClient).deleteExecution(TENANT_ID, "id1", AUTH_TOKEN);
    verify(keycloakClient).raisePriority(TENANT_ID, "id2", AUTH_TOKEN);
    verify(keycloakClient).updateRealm(eq(TENANT_ID), any(), eq(AUTH_TOKEN));
  }

  @Test
  void addCustomAuthFlowForCentralTenantExecutionNotFound() {
    when(keycloakCredentialsService.getMasterAuthToken()).thenReturn("token");
    when(keycloakClient.getExecutions(anyString(), anyString(), anyString())).thenReturn(List.of());

    assertThrows(IllegalStateException.class, () -> keycloakService.addCustomAuthFlowForCentralTenant(TENANT_ID));
  }

  private static String getTenantClientAlias(String tenant) {
    return tenant + "-keycloak-oidc";
  }

}
