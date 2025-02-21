package org.folio.consortia.service;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.folio.consortia.client.KeycloakClient;
import org.folio.consortia.config.keycloak.KeycloakLoginClientProperties;
import org.folio.consortia.config.keycloak.KeycloakProperties;
import org.folio.consortia.domain.dto.KeycloakClientCredentials;
import org.folio.consortia.domain.dto.KeycloakTokenResponse;
import org.folio.consortia.service.impl.KeycloakCredentialsServiceImpl;
import org.folio.consortia.support.CopilotGenerated;
import org.folio.tools.store.SecureStore;
import org.folio.tools.store.exception.NotFoundException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Value;

import java.util.List;

@CopilotGenerated(partiallyGenerated = true)
@ExtendWith(MockitoExtension.class)
class KeycloakCredentialsServiceTest {

  private static final String AUTH_TOKEN = "Bearer token";

  @Mock
  private KeycloakClient keycloakClient;
  @Mock
  private KeycloakProperties keycloakProperties;
  @Mock
  private KeycloakLoginClientProperties keycloakClientProperties;
  @Mock
  private SecureStore secureStore;

  @InjectMocks
  @Spy
  private KeycloakCredentialsServiceImpl keycloakCredentialsService;

  @Value("${folio.environment}")
  private String folioEnvironment;

  @Test
  void getClientCredentials_returnsValidCredentials() {
    String memberTenant = "member";
    when(keycloakClientProperties.getClientNameSuffix()).thenReturn("-suffix");
    when(keycloakClient.getClientCredentials(memberTenant, memberTenant + "-suffix", AUTH_TOKEN))
      .thenReturn(List.of(createClientCredentials(memberTenant + "-suffix", "secret", true)));

    KeycloakClientCredentials credentials = keycloakCredentialsService.getClientCredentials(memberTenant, AUTH_TOKEN);

    assertEquals("member-suffix", credentials.getClientId());
    assertEquals("secret", credentials.getSecret());
    verify(keycloakClient).getClientCredentials(memberTenant, "member-suffix", AUTH_TOKEN);
  }

  @Test
  void getClientCredentials_throwsException_clientNotFound() {
    String memberTenant = "member";
    when(keycloakClientProperties.getClientNameSuffix()).thenReturn("-suffix");
    when(keycloakClient.getClientCredentials(memberTenant, memberTenant + "-suffix", AUTH_TOKEN))
      .thenReturn(List.of());

    assertThrows(IllegalStateException.class, () -> keycloakCredentialsService.getClientCredentials(memberTenant, AUTH_TOKEN));
    verify(keycloakClient).getClientCredentials(memberTenant, "member-suffix", AUTH_TOKEN);
  }

  @Test
  void getClientCredentials_throwsException_clientNotEnabled() {
    String memberTenant = "member";
    when(keycloakClientProperties.getClientNameSuffix()).thenReturn("-suffix");
    when(keycloakClient.getClientCredentials(memberTenant, memberTenant + "-suffix", AUTH_TOKEN))
      .thenReturn(List.of(createClientCredentials(memberTenant + "-suffix", "secret", false)));

    assertThrows(IllegalStateException.class, () -> keycloakCredentialsService.getClientCredentials(memberTenant, AUTH_TOKEN));
    verify(keycloakClient).getClientCredentials(memberTenant, "member-suffix", AUTH_TOKEN);
  }

  @Test
  void getMasterAuthToken_returnsValidToken() {
    String clientId = "clientId";
    String clientSecret = "clientSecret";
    var tokenResponse = createTokenResponse();

    when(keycloakProperties.getClientId()).thenReturn(clientId);
    when(secureStore.get(anyString())).thenReturn(clientSecret);
    when(keycloakClient.login(anyMap())).thenReturn(tokenResponse);

    String token = keycloakCredentialsService.getMasterAuthToken();

    assertEquals(AUTH_TOKEN, token);
    verify(secureStore).get("%s_%s_%s".formatted(folioEnvironment, "master", clientId));
  }

  @Test
  void getMasterAuthToken_throwsException_secretNotFound() {
    String clientId = "clientId";
    when(keycloakProperties.getClientId()).thenReturn(clientId);
    when(secureStore.get(anyString())).thenThrow(new NotFoundException("Not found"));

    assertThrows(IllegalStateException.class, () -> keycloakCredentialsService.getMasterAuthToken());
    verify(secureStore).get("%s_%s_%s".formatted(folioEnvironment, "master", clientId));
  }

  private static KeycloakTokenResponse createTokenResponse() {
    KeycloakTokenResponse tokenResponse = new KeycloakTokenResponse();
    tokenResponse.setTokenType(AUTH_TOKEN.split(" ")[0]);
    tokenResponse.setAccessToken(AUTH_TOKEN.split(" ")[1]);
    tokenResponse.setExpiresIn(3600L);
    return tokenResponse;
  }

  public static KeycloakClientCredentials createClientCredentials(String clientId, String secret, boolean enabled) {
    return KeycloakClientCredentials.builder()
      .clientId(clientId)
      .secret(secret)
      .enabled(enabled)
      .build();
  }

}
