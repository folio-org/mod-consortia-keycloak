package org.folio.consortia.utils;

import org.folio.consortia.domain.dto.KeycloakIdentityProvider;

import lombok.experimental.UtilityClass;

@UtilityClass
public class KeycloakUtils {

  private static final String REALM_URL = "%s/realms/%s";
  private static final String AUTHORIZATION_URL = "%s/protocol/openid-connect/auth";
  private static final String TOKEN_URL = "%s/protocol/openid-connect/token";
  private static final String LOGOUT_URL = "%s/protocol/openid-connect/logout";
  private static final String USER_INFO_URL = "%s/protocol/openid-connect/userinfo";
  private static final String JWKS_URL = "%s/protocol/openid-connect/certs";

  public static KeycloakIdentityProvider.ClientConfig buildIdpClientConfig(String baseUrl, String tenantRealm, String clientId, String clientSecret) {
    var realmUrl = REALM_URL.formatted(baseUrl, tenantRealm);
    return KeycloakIdentityProvider.ClientConfig.builder()
      .clientId(clientId)
      .clientSecret(clientSecret)
      .clientAuthMethod("client_secret_post")
      .authorizationUrl(AUTHORIZATION_URL.formatted(realmUrl))
      .tokenUrl(TOKEN_URL.formatted(realmUrl))
      .logoutUrl(LOGOUT_URL.formatted(realmUrl))
      .userInfoUrl(USER_INFO_URL.formatted(realmUrl))
      .issuer(realmUrl)
      .jwksUrl(JWKS_URL.formatted(realmUrl))
      .validateSignature(true)
      .useJwksUrl(true)
      .pkceEnabled(false)
      .build();
  }

}
