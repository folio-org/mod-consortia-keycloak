package org.folio.consortia.service;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.util.List;
import java.util.Map;
import org.folio.consortia.client.KeycloakClient;
import org.folio.consortia.domain.dto.RealmExecutions;
import org.folio.consortia.domain.dto.Tenant;
import org.folio.consortia.service.impl.KeycloakServiceImpl;
import org.folio.consortia.support.CopilotGenerated;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
@CopilotGenerated(partiallyGenerated = true)
class KeycloakServiceTest {
  private static final String AUTH_TOKEN = "dGVzdC10b2tlbg==";

  @InjectMocks
  private KeycloakServiceImpl keycloakService;
  @Mock
  private KeycloakClient keycloakClient;
  @Mock
  private TokenService tokenService;
  @Captor
  private ArgumentCaptor<Map<String, String>> mapCaptor;

  @Test
  void addCustomAuthFlowForCentralTenantSuccess() {
    Tenant tenant = new Tenant();
    tenant.setId("tenant-id");
    tenant.setIsCentral(true);
    var executions = List.of(
      new RealmExecutions().withId("id1").withProviderId("auth-username-password-form"),
      new RealmExecutions().withId("id2").withProviderId("ecs-folio-auth-usrnm-pwd-form"));
    var realm = new ObjectNode(JsonNodeFactory.instance);

    when(tokenService.issueToken()).thenReturn(AUTH_TOKEN);
    when(keycloakClient.getExecutions(anyString(), anyString(), anyString())).thenReturn(executions);
    when(keycloakClient.getRealm(anyString(), anyString())).thenReturn(realm);

    keycloakService.addCustomAuthFlowForCentralTenant(tenant);

    verify(keycloakClient).copyBrowserFlow(eq("tenant-id"), mapCaptor.capture(), eq(AUTH_TOKEN));
    verify(keycloakClient).executeBrowserFlow(eq("tenant-id"), eq("custom-browser%20forms"), mapCaptor.capture(), eq(AUTH_TOKEN));
    verify(keycloakClient).deleteExecution(eq("tenant-id"), eq("id1"), eq(AUTH_TOKEN));
    verify(keycloakClient).raisePriority(eq("tenant-id"), eq("id2"), eq(AUTH_TOKEN));
    verify(keycloakClient).updateRealm(eq("tenant-id"), any(), eq(AUTH_TOKEN));
  }

  @Test
  void addCustomAuthFlowForCentralTenantNotCentral() {
    Tenant tenant = new Tenant();
    tenant.setId("tenant-id");
    tenant.setIsCentral(false);

    keycloakService.addCustomAuthFlowForCentralTenant(tenant);

    verifyNoInteractions(keycloakClient);
    verifyNoInteractions(tokenService);
  }

  @Test
  void addCustomAuthFlowForCentralTenantExecutionNotFound() {
    Tenant tenant = new Tenant();
    tenant.setId("tenant-id");
    tenant.setIsCentral(true);
    String token = "token";

    when(tokenService.issueToken()).thenReturn(token);
    when(keycloakClient.getExecutions(anyString(), anyString(), anyString())).thenReturn(List.of());

    assertThrows(IllegalStateException.class, () -> keycloakService.addCustomAuthFlowForCentralTenant(tenant));
  }
}
