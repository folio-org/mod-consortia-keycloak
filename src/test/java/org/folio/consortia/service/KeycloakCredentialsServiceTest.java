package org.folio.consortia.service;

import static org.awaitility.Awaitility.await;
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
import org.springframework.scheduling.TaskScheduler;
import org.springframework.scheduling.concurrent.SimpleAsyncTaskScheduler;

import java.time.Instant;
import java.util.concurrent.TimeUnit;

@CopilotGenerated(partiallyGenerated = true)
@ExtendWith(MockitoExtension.class)
class KeycloakCredentialsServiceTest {

  @Mock
  private KeycloakClient keycloakClient;
  @Mock
  private KeycloakProperties keycloakProperties;
  @Mock
  private KeycloakLoginClientProperties keycloakClientProperties;
  @Mock
  private SecureStore secureStore;
  @Spy
  private TaskScheduler asyncTaskScheduler = new SimpleAsyncTaskScheduler();

  @InjectMocks
  @Spy
  private KeycloakCredentialsServiceImpl keycloakCredentialsService;

  @Value("${folio.environment}")
  private String folioEnvironment;

  @Test
  void getClientCredentials_returnsValidCredentials() {
    String centralTenant = "central";
    String memberTenant = "member";
    when(keycloakClientProperties.getClientNameSuffix()).thenReturn("-suffix");
    when(secureStore.get(anyString())).thenReturn("secret");

    KeycloakClientCredentials credentials = keycloakCredentialsService.getClientCredentials(centralTenant, memberTenant);

    assertEquals("member-suffix", credentials.clientId());
    assertEquals("secret", credentials.clientSecret());
    verify(secureStore).get("%s_%s_%s".formatted(folioEnvironment, "central", "member-suffix"));
  }

  @Test
  void getClientCredentials_returnsValidCredentials_secureStoreDisabled() {
    String centralTenant = "central";
    String memberTenant = "member";
    when(keycloakClientProperties.getClientNameSuffix()).thenReturn("-suffix");
    when(keycloakClientProperties.getSecureStoreDisabled()).thenReturn(true);

    KeycloakClientCredentials credentials = keycloakCredentialsService.getClientCredentials(centralTenant, memberTenant);

    assertEquals("member-suffix", credentials.clientId());
    assertEquals("SecretPassword", credentials.clientSecret());
    verify(secureStore, never()).get("%s_%s_%s".formatted(folioEnvironment, "central", "member-suffix"));
  }

  @Test
  void getClientCredentials_throwsExceptionIfSecretNotFound() {
    String centralTenant = "central";
    String memberTenant = "member";
    when(keycloakClientProperties.getClientNameSuffix()).thenReturn("-suffix");
    when(secureStore.get(anyString())).thenThrow(new NotFoundException("Not found"));

    assertThrows(IllegalStateException.class, () -> keycloakCredentialsService.getClientCredentials(centralTenant, memberTenant));
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

    assertEquals("Bearer accessToken", token);
    verify(asyncTaskScheduler).schedule(any(Runnable.class), any(Instant.class));
  }

  @Test
  void getMasterAuthToken_evictsTokenAfterExpiry() {
    String clientId = "clientId";
    String clientSecret = "clientSecret";
    var tokenResponse = createTokenResponse();
    tokenResponse.setExpiresIn(65L);

    when(keycloakProperties.getClientId()).thenReturn(clientId);
    when(secureStore.get(anyString())).thenReturn(clientSecret);
    when(keycloakClient.login(anyMap())).thenReturn(tokenResponse);

    String token = keycloakCredentialsService.getMasterAuthToken();

    assertEquals("Bearer accessToken", token);
    verify(asyncTaskScheduler).schedule(any(Runnable.class), any(Instant.class));
    await().atMost(10, TimeUnit.SECONDS).untilAsserted(() -> {
      verify(keycloakCredentialsService).evictMasterAuthToken();
    });
  }

  private static KeycloakTokenResponse createTokenResponse() {
    KeycloakTokenResponse tokenResponse = new KeycloakTokenResponse();
    tokenResponse.setTokenType("Bearer");
    tokenResponse.setAccessToken("accessToken");
    tokenResponse.setExpiresIn(3600L);
    return tokenResponse;
  }

}
