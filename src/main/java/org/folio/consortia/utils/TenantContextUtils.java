package org.folio.consortia.utils;

import lombok.experimental.UtilityClass;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.SerializationUtils;
import org.folio.spring.DefaultFolioExecutionContext;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.integration.XOkapiHeaders;
import org.folio.spring.scope.FolioExecutionContextSetter;
import org.springframework.messaging.MessageHeaders;

import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@UtilityClass
@Log4j2
public class TenantContextUtils {
  public static FolioExecutionContext getFolioExecutionContextCopyForTenant(FolioExecutionContext context, String tenant) {
    var headers = context.getAllHeaders() != null
      ? context.getAllHeaders()
      : new HashMap<String, Collection<String>>();
    headers.put(XOkapiHeaders.TENANT, Collections.singletonList(tenant));

    return new DefaultFolioExecutionContext(context.getFolioModuleMetadata(), headers);
  }

  public static FolioExecutionContext createFolioExecutionContext(MessageHeaders headers, FolioModuleMetadata moduleMetadata,
                                                                  String centralTenantId) {
    return getContextFromKafkaHeaders(headers, moduleMetadata, centralTenantId);
  }

  /**
   * This method change tenant(x-okapi-tenant: tenantId) of context to new tenant and return new context with tenantId.
   *
   * @param tenantId new tenantId
   * @param context current context
   * @param folioModuleMetadata current module metadata
   * @return new context with new tenantId
   */
  public static FolioExecutionContext prepareContextForTenant(String tenantId, FolioModuleMetadata folioModuleMetadata, FolioExecutionContext context) {
    if (MapUtils.isNotEmpty(context.getOkapiHeaders())) {
      // create deep copy of headers in order to make switching context thread safe
      var headersCopy = SerializationUtils.clone((HashMap<String, Collection<String>>) context.getAllHeaders());
      headersCopy.put(XOkapiHeaders.TENANT, List.of(tenantId));
      log.info("FOLIO context initialized with tenant {}", tenantId);
      return new DefaultFolioExecutionContext(folioModuleMetadata, headersCopy);
    }
    throw new IllegalStateException("Okapi headers not provided");
  }

  public static void runInFolioContext(FolioExecutionContext context, Runnable runnable) {
    try (var fec = new FolioExecutionContextSetter(context)) {
      runnable.run();
    }
  }

  private static FolioExecutionContext getContextFromKafkaHeaders(MessageHeaders headers,
                                                                  FolioModuleMetadata moduleMetadata, String centralTenantId) {
    Map<String, Collection<String>> map = new HashMap<>();
    map.put(XOkapiHeaders.TENANT, List.of(centralTenantId));
    map.put(XOkapiHeaders.URL, getHeaderValue(headers, XOkapiHeaders.URL, null));
    map.put(XOkapiHeaders.TOKEN, getHeaderValue(headers, XOkapiHeaders.TOKEN, null));
    map.put(XOkapiHeaders.USER_ID, getHeaderValue(headers, XOkapiHeaders.USER_ID, null));

    return new DefaultFolioExecutionContext(moduleMetadata, map);
  }

  public static List<String> getHeaderValue(MessageHeaders headers, String headerName, String defaultValue) {
    var headerValue = headers.get(headerName);
    var value = headerValue == null
      ? defaultValue
      : new String((byte[]) headerValue, StandardCharsets.UTF_8);
    return value == null ? Collections.emptyList() : Collections.singletonList(value);
  }

  public static String getTenantIdFromHeader(FolioExecutionContext folioExecutionContext) {
    String tenantId;
    if (MapUtils.isNotEmpty(folioExecutionContext.getOkapiHeaders())) {
      tenantId = folioExecutionContext.getOkapiHeaders().get(XOkapiHeaders.TENANT).iterator().next();
      return tenantId;
    }
    throw new IllegalStateException("Tenant is not available in header");
  }
}
