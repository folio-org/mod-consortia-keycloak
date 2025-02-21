package org.folio.consortia.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Objects;

import org.folio.consortia.client.KeycloakClient;
import org.folio.consortia.config.keycloak.KeycloakProperties;
import org.folio.consortia.domain.dto.KeycloakRealmConfiguration;
import org.folio.consortia.domain.dto.TokenResponse;
import org.folio.consortia.support.CopilotGenerated;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.concurrent.ConcurrentMapCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.ContextConfiguration;

@SpringBootTest
@ContextConfiguration(classes = TokenServiceTest.TestConfig.class)
@CopilotGenerated(partiallyGenerated = true)
class TokenServiceTest {

  @MockBean
  private KeycloakClient keycloakClient;

  @MockBean
  private KeycloakProperties keycloakProperties;

  @MockBean
  private RealmConfigurationProvider realmConfigurationProvider;

  @Autowired
  private CacheManager cacheManager;

  @Autowired
  private TokenService tokenService;

  @BeforeEach
  void setUp() {
    // Clear the cache before each test
    Objects.requireNonNull(cacheManager.getCache("token")).clear();

    // Setup mock responses
    var realmConfig = new KeycloakRealmConfiguration();
    realmConfig.setClientId("client-id");
    realmConfig.setClientSecret("client-secret");

    when(realmConfigurationProvider.getRealmConfiguration()).thenReturn(realmConfig);
    when(keycloakProperties.getClientId()).thenReturn("client-id");
    when(keycloakProperties.getGrantType()).thenReturn("client_credentials");
  }

  @Test
  void issueToken_shouldCacheToken() {
    var tokenResponse = new TokenResponse();
    tokenResponse.setAccessToken("access-token");
    tokenResponse.setTokenType("Bearer");
    when(keycloakClient.login(any())).thenReturn(tokenResponse);

    String firstCall = tokenService.issueToken();

    String secondCall = tokenService.issueToken();

    assertThat(firstCall).isEqualTo("Bearer access-token");
    assertThat(secondCall).isEqualTo("Bearer access-token");

    // Verify that keycloakClient.login was called only once due to caching
    verify(keycloakClient, times(1)).login(any());

    // Verify token is in cache
    String cachedToken = Objects.requireNonNull(cacheManager.getCache("token")).get("admin-cli-token", String.class);
    assertThat(cachedToken).isEqualTo("Bearer access-token");
  }

  @Test
  void renewToken_shouldUpdateCache() {
    var initialToken = new TokenResponse();
    initialToken.setAccessToken("initial-token");
    initialToken.setTokenType("Bearer");

    var newToken = new TokenResponse();
    newToken.setAccessToken("new-token");
    newToken.setTokenType("Bearer");

    when(keycloakClient.login(any()))
      .thenReturn(initialToken)
      .thenReturn(newToken);

    tokenService.issueToken();

    // Renew token (should update cache)
    String renewedToken = tokenService.renewToken();

    String cachedToken = tokenService.issueToken();

    assertThat(renewedToken).isEqualTo("Bearer new-token");
    assertThat(cachedToken).isEqualTo("Bearer new-token");

    verify(keycloakClient, times(2)).login(any());

    // Verify cache contains the renewed token
    String tokenFromCache = Objects.requireNonNull(cacheManager.getCache("token")).get("admin-cli-token", String.class);
    assertThat(tokenFromCache).isEqualTo("Bearer new-token");
  }

  @Configuration
  @EnableCaching
  static class TestConfig {
    @Bean
    public CacheManager cacheManager() {
      return new ConcurrentMapCacheManager("token");
    }

    @Bean
    public TokenService tokenService(KeycloakClient keycloakClient,
                                     KeycloakProperties keycloakProperties,
                                     RealmConfigurationProvider realmConfigurationProvider) {
      return new TokenService(keycloakClient, keycloakProperties, realmConfigurationProvider);
    }
  }
}
