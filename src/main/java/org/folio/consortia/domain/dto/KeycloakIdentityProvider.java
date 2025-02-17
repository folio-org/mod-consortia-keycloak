package org.folio.consortia.domain.dto;

import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class KeycloakIdentityProvider {

  private String alias;
  private String displayName;
  private String providerId;
  private boolean enabled;
  private ClientConfig config;

  @AllArgsConstructor
  @NoArgsConstructor
  @Data
  @Builder
  public static class ClientConfig {
    private String clientId;
    private String clientSecret;
    private String clientAuthMethod;

    private String authorizationUrl;
    private String tokenUrl;
    private String logoutUrl;
    private String userInfoUrl;
    private String issuer;
    private String jwksUrl;

    private boolean validateSignature;
    private boolean useJwksUrl;
    private boolean pkceEnabled;
  }

}
