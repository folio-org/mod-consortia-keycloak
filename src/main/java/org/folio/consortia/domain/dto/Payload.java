package org.folio.consortia.domain.dto;

import java.util.UUID;
import lombok.Data;
import com.fasterxml.jackson.annotation.JsonProperty;

@Data
public class Payload {

  @JsonProperty("user_id")
  private UUID userId;

  private String sub;

}
