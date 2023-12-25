package org.folio.consortia.domain.dto;

import lombok.Getter;

@Getter
public enum UserType {
  PATRON("patron"),
  STAFF("staff"),
  SHADOW("shadow"),
  SYSTEM("system");

  UserType(String name) {
    this.name = name;
  }

  private final String name;
}
