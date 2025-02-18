package org.folio.consortia.config.keycloak;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Data
@Component
@Validated
@ConfigurationProperties(prefix = "folio.keycloak.login")
public class KeycloakLoginClientProperties {
  @NotNull
  private String clientNameSuffix;
  @NotNull
  private Boolean secureStoreDisabled;
}
