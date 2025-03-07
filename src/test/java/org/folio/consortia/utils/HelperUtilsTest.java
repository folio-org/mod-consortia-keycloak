package org.folio.consortia.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.UUID;

import org.folio.consortia.domain.dto.UserTenant;
import org.folio.consortia.support.CopilotGenerated;
import org.junit.jupiter.api.Test;

@CopilotGenerated
public class HelperUtilsTest {

  @Test
  void checkIdenticalOrThrow_IdenticalStrings_NoExceptionThrown() {
    HelperUtils.checkIdenticalOrThrow("test", "test", "Strings are not identical");
  }

  @Test
  void checkIdenticalOrThrow_DifferentStrings_ExceptionThrown() {
    assertThrows(IllegalArgumentException.class, () ->
      HelperUtils.checkIdenticalOrThrow("test", "different", "Strings are not identical"));
  }

  @Test
  void randomString_GeneratesStringOfCorrectLength() {
    String randomStr = HelperUtils.randomString(10);
    assertEquals(10, randomStr.length());
  }

  @Test
  void randomString_GeneratesEmptyStringForZeroLength() {
    String randomStr = HelperUtils.randomString(0);
    assertEquals(0, randomStr.length());
  }

  @Test
  void createDummyUserTenant_CreatesUserTenantWithCorrectValues() {
    String username = "user";
    String tenantId = "tenant";
    String centralTenantId = "centralTenant";
    UUID consortiumId = UUID.randomUUID();

    UserTenant userTenant = HelperUtils.createDummyUserTenant(username, tenantId, centralTenantId, consortiumId);

    assertEquals(username, userTenant.getUsername());
    assertEquals(tenantId, userTenant.getTenantId());
    assertEquals(centralTenantId, userTenant.getCentralTenantId());
    assertEquals(consortiumId, userTenant.getConsortiumId());
  }
}
