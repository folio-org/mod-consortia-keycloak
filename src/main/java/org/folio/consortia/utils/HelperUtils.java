package org.folio.consortia.utils;

import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.RandomStringGenerator;
import org.folio.consortia.domain.dto.UserTenant;

import lombok.experimental.UtilityClass;

@UtilityClass
public class HelperUtils {

  private static final Integer RANDOM_STRING_COUNT = 5;

  public static void checkIdenticalOrThrow(String firstString, String secondString, String errorMsg) {
    if (!StringUtils.equals(firstString, secondString)) {
      throw new IllegalArgumentException(errorMsg);
    }
  }

  public static String generateShadowUsername(String realUsername) {
    RandomStringGenerator generator = new RandomStringGenerator.Builder().withinRange('a', 'z').get();
    return "%s_%s".formatted(realUsername, generator.generate(RANDOM_STRING_COUNT));
  }

  public static String generateShadowUsernameOrDefault(String realUsername, String shadowUsername) {
    var shadowUsernameOriginal = StringUtils.substring(shadowUsername, 0, shadowUsername.length() - RANDOM_STRING_COUNT - 1);
    return shadowUsernameOriginal.equals(realUsername)
      ? shadowUsername
      : generateShadowUsername(realUsername);
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
