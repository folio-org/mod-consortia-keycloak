package org.folio.consortia.config.keycloak;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Data
@Component
@Validated
@ConfigurationProperties(prefix = "folio.keycloak.identity-provider")
public class KeycloakIdentityProviderProperties {
  @NotNull
  private Boolean enabled;
  @NotNull
  private String baseUrl;
  @NotNull
  private String alias;
  @NotNull
  private String displayName;
}
