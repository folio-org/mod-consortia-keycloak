package org.folio.consortia.utils;

import org.folio.consortia.domain.dto.KeycloakIdentityProvider;
import org.folio.consortia.support.CopilotGenerated;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

@CopilotGenerated
class KeycloakUtilsTest {

  @Test
  void buildIdpClientConfig_createsValidConfig() {
    String baseUrl = "http://localhost";
    String tenantRealm = "testRealm";
    String clientId = "testClient";
    String clientSecret = "testSecret";

    KeycloakIdentityProvider.ClientConfig config = KeycloakUtils.buildIdpClientConfig(baseUrl, tenantRealm, clientId, clientSecret);

    assertEquals(clientId, config.getClientId());
    assertEquals(clientSecret, config.getClientSecret());
    assertEquals("client_secret_post", config.getClientAuthMethod());
    assertEquals("http://localhost/realms/testRealm/protocol/openid-connect/auth", config.getAuthorizationUrl());
    assertEquals("http://localhost/realms/testRealm/protocol/openid-connect/token", config.getTokenUrl());
    assertEquals("http://localhost/realms/testRealm/protocol/openid-connect/logout", config.getLogoutUrl());
    assertEquals("http://localhost/realms/testRealm/protocol/openid-connect/userinfo", config.getUserInfoUrl());
    assertEquals("http://localhost/realms/testRealm", config.getIssuer());
    assertEquals("http://localhost/realms/testRealm/protocol/openid-connect/certs", config.getJwksUrl());
    assertTrue(config.isValidateSignature());
    assertTrue(config.isUseJwksUrl());
    assertFalse(config.isPkceEnabled());
  }

  @Test
  void formatTenantField_replacesTenantId() {
    String template = "tenant-{tenantId}-field";
    String tenantId = "12345";

    String result = KeycloakUtils.formatTenantField(template, tenantId);

    assertEquals("tenant-12345-field", result);
  }

  @Test
  void formatTenantField_handlesEmptyTenantId() {
    String template = "tenant-{tenantId}-field";
    String tenantId = "";

    String result = KeycloakUtils.formatTenantField(template, tenantId);

    assertEquals("tenant--field", result);
  }

  @Test
  void formatTenantField_handlesNoPlaceholder() {
    String template = "tenant-field";
    String tenantId = "12345";

    String result = KeycloakUtils.formatTenantField(template, tenantId);

    assertEquals("tenant-field", result);
  }

}
