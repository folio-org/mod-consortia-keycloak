package org.folio.consortia.service;

import static org.folio.consortia.support.EntityUtils.CENTRAL_TENANT_ID;
import static org.folio.consortia.support.EntityUtils.TENANT_ID;
import static org.mockito.Mockito.*;

import org.folio.consortia.client.KeycloakClient;
import org.folio.consortia.config.keycloak.KeycloakIdentityProviderProperties;
import org.folio.consortia.config.keycloak.KeycloakLoginClientProperties;
import org.folio.consortia.domain.dto.KeycloakIdentityProvider;
import org.folio.consortia.service.impl.KeycloakServiceImpl;
import org.folio.consortia.support.CopilotGenerated;
import org.folio.tools.store.SecureStore;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.batch.BatchAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.mock.mockito.SpyBean;

@SpringBootTest
@EnableAutoConfiguration(exclude = BatchAutoConfiguration.class)
@CopilotGenerated(partiallyGenerated = true)
class KeycloakServiceTest {

  @MockBean
  private KeycloakClient keycloakClient;
  @MockBean
  private SecureStore secureStore;
  @SpyBean
  private KeycloakIdentityProviderProperties keycloakIdpProperties;
  @SpyBean
  private KeycloakLoginClientProperties keycloakLoginClientProperties;

  @Autowired
  private KeycloakServiceImpl keycloakService;
  @Value("${folio.environment}")
  private String folioEnvironment;

  @BeforeEach
  void setUp() {
    when(keycloakIdpProperties.getEnabled()).thenReturn(true);
  }

  @Test
  void createIdentityProvider_createsProviderSuccessfully() {
    var alias = getTenantClientAlias(TENANT_ID);
    when(keycloakClient.getIdentityProvider(CENTRAL_TENANT_ID, alias)).thenReturn(null);
    when(keycloakLoginClientProperties.getSecureStoreDisabled()).thenReturn(true);

    keycloakService.createIdentityProvider(CENTRAL_TENANT_ID, TENANT_ID);

    verify(keycloakClient).getIdentityProvider(CENTRAL_TENANT_ID, alias);
    verify(keycloakClient).createIdentityProvider(eq(CENTRAL_TENANT_ID), any(KeycloakIdentityProvider.class));
  }

  @Test
  void createIdentityProvider_createsProviderSuccessfully_secureStoreEnabled() {
    var alias = getTenantClientAlias(TENANT_ID);
    var clientId = TENANT_ID + keycloakLoginClientProperties.getClientNameSuffix();
    when(keycloakClient.getIdentityProvider(CENTRAL_TENANT_ID, alias)).thenReturn(null);
    when(keycloakLoginClientProperties.getSecureStoreDisabled()).thenReturn(false);
    when(secureStore.get("%s_%s_%s".formatted(folioEnvironment, CENTRAL_TENANT_ID, clientId))).thenReturn("secret");

    keycloakService.createIdentityProvider(CENTRAL_TENANT_ID, TENANT_ID);

    verify(keycloakClient).getIdentityProvider(CENTRAL_TENANT_ID, alias);
    verify(keycloakClient).createIdentityProvider(eq(CENTRAL_TENANT_ID), any(KeycloakIdentityProvider.class));
  }

  @Test
  void createIdentityProvider_skipsIfDisabled() {
    var alias = getTenantClientAlias(TENANT_ID);
    when(keycloakIdpProperties.getEnabled()).thenReturn(false);

    keycloakService.createIdentityProvider(CENTRAL_TENANT_ID, TENANT_ID);

    verify(keycloakClient, never()).getIdentityProvider(CENTRAL_TENANT_ID, alias);
    verify(keycloakClient, never()).createIdentityProvider(anyString(), any(KeycloakIdentityProvider.class));
  }

  @Test
  void createIdentityProvider_skipsIfProviderExists() {
    var alias = getTenantClientAlias(TENANT_ID);
    when(keycloakClient.getIdentityProvider(CENTRAL_TENANT_ID, alias)).thenReturn(new KeycloakIdentityProvider());

    keycloakService.createIdentityProvider(CENTRAL_TENANT_ID, TENANT_ID);

    verify(keycloakClient).getIdentityProvider(CENTRAL_TENANT_ID, alias);
    verify(keycloakClient, never()).createIdentityProvider(anyString(), any(KeycloakIdentityProvider.class));
  }

  @Test
  void deleteIdentityProvider_deletesProviderSuccessfully() {
    keycloakService.deleteIdentityProvider(CENTRAL_TENANT_ID, TENANT_ID);

    verify(keycloakClient).deleteIdentityProvider(CENTRAL_TENANT_ID, getTenantClientAlias(TENANT_ID));
  }

  @Test
  void deleteIdentityProvider_skipsIfDisabled() {
    when(keycloakIdpProperties.getEnabled()).thenReturn(false);

    keycloakService.deleteIdentityProvider(CENTRAL_TENANT_ID, TENANT_ID);

    verify(keycloakClient, never()).deleteIdentityProvider(anyString(), anyString());
  }

  private static String getTenantClientAlias(String tenant) {
    return tenant + "-keycloak-oidc";
  }

}
