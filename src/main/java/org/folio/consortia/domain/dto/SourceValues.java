package org.folio.consortia.domain.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum SourceValues {
  CONSORTIUM("CONSORTIUM", "CONSORTIUM", "consortium"),
  USER("USER", "REGULAR", "user");

  private final String policyValue, roleValue, settingsValue;
}
