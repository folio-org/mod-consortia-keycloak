package org.folio.consortia.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.UUID;

import org.folio.consortia.domain.dto.UserTenant;
import org.folio.consortia.support.CopilotGenerated;
import org.junit.jupiter.api.Test;

@CopilotGenerated
class HelperUtilsTest {

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
  void generateShadowUsername_GeneratesCorrectString() {
    String randomStr = HelperUtils.generateShadowUsername("username");
    assertEquals(14, randomStr.length());
    assertTrue(randomStr.matches("username_[a-z]{5}"));
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
