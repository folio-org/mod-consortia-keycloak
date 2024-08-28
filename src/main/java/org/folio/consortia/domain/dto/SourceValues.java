package org.folio.consortia.domain.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum SourceValues {
  CONSORTIUM_FOLIO_INSTANCE("CONSORTIUM-FOLIO"),
  CONSORTIUM_MARC_INSTANCE("CONSORTIUM-MARC"),
  CONSORTIUM_LINKED_DATA_INSTANCE("CONSORTIUM-LINKED_DATA"),
  CONSORTIUM("consortium"),
  USER("user");

  private final String value;
}
