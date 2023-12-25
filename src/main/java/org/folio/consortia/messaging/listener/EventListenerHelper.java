package org.folio.consortia.messaging.listener;

import org.folio.consortia.service.ConsortiaConfigurationService;
import org.folio.consortia.utils.TenantContextUtils;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.integration.XOkapiHeaders;
import org.folio.spring.scope.FolioExecutionContextSetter;
import org.springframework.dao.InvalidDataAccessResourceUsageException;
import org.springframework.messaging.MessageHeaders;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Log4j2
@Component
@RequiredArgsConstructor
public class EventListenerHelper {
  private final ConsortiaConfigurationService configurationService;
  private final FolioModuleMetadata folioMetadata;

  protected String getCentralTenantByIdByHeader(MessageHeaders messageHeaders) {
    String requestedTenantId = TenantContextUtils.getHeaderValue(messageHeaders, XOkapiHeaders.TENANT, null).get(0);
    // getting central tenant from its own table by using appropriate context
    try (var ignored = new FolioExecutionContextSetter(
      TenantContextUtils.createFolioExecutionContext(messageHeaders, folioMetadata, requestedTenantId))) {
      return configurationService.getCentralTenantId(requestedTenantId);
    } catch (InvalidDataAccessResourceUsageException e) {
      log.info("Table consortia_configuration is not exists, because tenant: {} is not in consortium, DB message: {}, skipping...",
        requestedTenantId, e.getMessage());
    }
    return null;
  }
}
