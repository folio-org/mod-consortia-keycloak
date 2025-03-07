package org.folio.consortia.utils;

import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.RandomStringGenerator;
import org.folio.consortia.domain.dto.UserTenant;

import lombok.experimental.UtilityClass;

@UtilityClass
public class HelperUtils {

  public static void checkIdenticalOrThrow(String firstString, String secondString, String errorMsg) {
    if (!StringUtils.equals(firstString, secondString)) {
      throw new IllegalArgumentException(errorMsg);
    }
  }

  public static String randomString(Integer noOfString) {
    RandomStringGenerator generator = new RandomStringGenerator.Builder().withinRange('a', 'z').get();
    return generator.generate(noOfString);
  }

  public static UserTenant createDummyUserTenant(String username, String tenantId, String centralTenantId, UUID consortiumId) {
    return new UserTenant()
      .id(UUID.randomUUID())
      .userId(UUID.randomUUID())
      .username(username)
      .tenantId(tenantId)
      .centralTenantId(centralTenantId)
      .consortiumId(consortiumId);
  }
}
