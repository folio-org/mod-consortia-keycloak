package org.folio.consortia.config.property;

import jakarta.validation.constraints.NotNull;
import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

@Data
@Validated
@Component
@ConfigurationProperties("related-modules")
public class RelatedModulesProperties {

  @NotNull(message = "mod-users ID is required")
  private String modUsersId;
}
