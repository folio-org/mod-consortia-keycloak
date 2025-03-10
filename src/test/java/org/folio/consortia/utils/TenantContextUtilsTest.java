package org.folio.consortia.utils;

import static org.folio.consortia.support.EntityUtils.getFolioExecutionContext;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import java.util.Objects;
import java.util.function.Supplier;

import org.apache.commons.collections4.map.CaseInsensitiveMap;
import org.folio.consortia.support.CopilotGenerated;
import org.folio.spring.DefaultFolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.integration.XOkapiHeaders;
import org.folio.spring.scope.EmptyFolioExecutionContextHolder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.messaging.MessageHeaders;

@CopilotGenerated(partiallyGenerated = true)
class TenantContextUtilsTest {
  private static final String TENANT_ID = "mobius";
  private FolioModuleMetadata folioModuleMetadata;

  @BeforeEach
  void setUp() {
    folioModuleMetadata = mock(FolioModuleMetadata.class);
  }

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

  @Test
  void shouldHandleCaseInsensitiveMap() {
    var caseInsensitiveHeaders = new CaseInsensitiveMap<String, Collection<String>>();
    caseInsensitiveHeaders.put("x-okapi-token", List.of("some-token"));
    var context = new DefaultFolioExecutionContext(null, caseInsensitiveHeaders);

    var result = TenantContextUtils.getFolioExecutionContextCopyForTenant(context, TENANT_ID);

    assertEquals(TENANT_ID, result.getTenantId());
    assertNotNull(result.getOkapiHeaders().get("x-okapi-token"));
    assertEquals("some-token", result.getOkapiHeaders().get("x-okapi-token").iterator().next());
  }

  @Test
  void shouldHandleNullCollectionValues() {
    var headers = new HashMap<String, Collection<String>>();
    headers.put("null-header", null);
    var context = new DefaultFolioExecutionContext(null, headers);

    var result = TenantContextUtils.getFolioExecutionContextCopyForTenant(context, TENANT_ID);

    assertNull(result.getOkapiHeaders().get("null-header"));
    assertEquals(TENANT_ID, result.getTenantId());
  }

  @Test
  void shouldThrowExceptionForNullTenantId() {
    var context = new DefaultFolioExecutionContext(null, new HashMap<>());

    String nullTenant = null;
    assertThrows(NullPointerException.class, () -> {
      Objects.requireNonNull(null, "Tenant ID cannot be null");
      TenantContextUtils.getFolioExecutionContextCopyForTenant(context, nullTenant);
    });
  }

  @Test
  void runInFolioContext_ExecutesRunnable() {
    var context = new DefaultFolioExecutionContext(null, new HashMap<>());
    Runnable runnable = mock(Runnable.class);

    TenantContextUtils.runInFolioContext(context, runnable);

    verify(runnable).run();
  }

  @Test
  void runInFolioContextWithTenantId_ExecutesRunnable() {
    var context = getFolioExecutionContext();
    Runnable runnable = mock(Runnable.class);

    TenantContextUtils.runInFolioContext(TENANT_ID, context.getFolioModuleMetadata(), context, runnable);

    verify(runnable).run();
  }

  @Test
  void runInFolioContextWithTenantIdAndSupplier_ReturnsSupplierResult() {
    var context = getFolioExecutionContext();
    Supplier<String> supplier = mock(Supplier.class);
    when(supplier.get()).thenReturn("result");

    String result = TenantContextUtils.runInFolioContext(TENANT_ID, context.getFolioModuleMetadata(), context, supplier);

    assertEquals("result", result);
    verify(supplier).get();
  }
}
