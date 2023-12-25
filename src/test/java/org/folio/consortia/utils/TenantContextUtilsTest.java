package org.folio.consortia.utils;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.HashMap;
import java.util.Map;

import org.folio.spring.DefaultFolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.integration.XOkapiHeaders;
import org.folio.spring.scope.EmptyFolioExecutionContextHolder;
import org.junit.jupiter.api.Test;
import org.springframework.messaging.MessageHeaders;

class TenantContextUtilsTest {
  public static final String TENANT_ID = "mobius";
  FolioModuleMetadata folioModuleMetadata;
  @Test
  void shouldSetTenantIdIfHeadersPassed() {
    var context = new DefaultFolioExecutionContext(null, new HashMap<>());

    var result = TenantContextUtils.getFolioExecutionContextCopyForTenant(context, TENANT_ID);

    assertEquals(TENANT_ID, result.getTenantId());
  }

  @Test
  void shouldSetTenantIdIfNoHeadersPassed() {
    var context = new EmptyFolioExecutionContextHolder(null).getEmptyFolioExecutionContext();

    var result = TenantContextUtils.getFolioExecutionContextCopyForTenant(context, TENANT_ID);

    assertEquals(TENANT_ID, result.getTenantId());
  }

  @Test
  void shouldSetTenantIdIfNoHeadersPassed2() {
    Map<String, Object> header = new HashMap<>();
    header.put(XOkapiHeaders.TENANT, TENANT_ID.getBytes());

    var messageHeaders = new MessageHeaders(header);
    var result = TenantContextUtils.createFolioExecutionContext(messageHeaders, folioModuleMetadata, TENANT_ID);

    assertEquals(TENANT_ID, result.getTenantId());
  }
}
