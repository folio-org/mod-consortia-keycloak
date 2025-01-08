package org.folio.consortia.domain.dto;

import lombok.Getter;

@Getter
public enum UserType {
  PATRON("patron"),
  STAFF("staff"),
  SHADOW("shadow"),
  SYSTEM("system"),
  DCB("dcb");

  UserType(String name) {
    this.name = name;
  }

  private final String name;

  public static UserType fromName(String name) {
    for (UserType userType : values()) {
      if (userType.name.equals(name)) {
        return userType;
      }
    }
    return null;
  }
}
