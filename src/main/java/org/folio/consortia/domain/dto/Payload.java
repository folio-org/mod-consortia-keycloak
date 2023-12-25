package org.folio.consortia.domain.dto;

import lombok.Data;
import com.fasterxml.jackson.annotation.JsonProperty;

@Data
public class Payload {

  @JsonProperty("user_id")
  private String userId;

  private String sub;

}
