package org.folio.consortia.domain.dto;

public enum SourceValues {
  CONSORTIUM_FOLIO_INSTANCE("CONSORTIUM-FOLIO"),
  CONSORTIUM_MARC_INSTANCE("CONSORTIUM-MARC"),
  CONSORTIUM("consortium"),
  USER("user");

  private final String value;

  SourceValues(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }
}
