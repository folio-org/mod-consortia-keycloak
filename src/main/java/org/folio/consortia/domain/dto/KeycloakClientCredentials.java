package org.folio.consortia.domain.dto;

import java.io.Serial;
import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class KeycloakClientCredentials implements Serializable {

  @Serial
  private static final long serialVersionUID = -5450019006221767712L;

  private String id;
  private String clientId;
  private String secret;
  private Boolean enabled;

}
