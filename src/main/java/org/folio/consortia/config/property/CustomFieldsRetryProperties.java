package org.folio.consortia.config.property;

import jakarta.validation.constraints.NotNull;
import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;
import org.springframework.validation.annotation.Validated;

@Data
@Validated
@Component
@ConfigurationProperties("custom-fields")
public class CustomFieldsRetryProperties {

  @NotNull(message = "backoffDelay is required")
  private Long backoffDelay;
  @NotNull(message = "maxAttempts is required")
  private Integer maxAttempts;
}
