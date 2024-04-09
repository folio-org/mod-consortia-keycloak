package org.folio.consortia.messaging.listener;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.when;

import org.folio.consortia.service.ConsortiaConfigurationService;
import java.util.HashMap;
import java.util.Map;

import org.folio.consortia.base.BaseIT;
import org.folio.spring.integration.XOkapiHeaders;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.dao.InvalidDataAccessResourceUsageException;
import org.springframework.messaging.MessageHeaders;

@SpringBootTest
class EventListenerHelperTest {
  @InjectMocks
  private EventListenerHelper eventListenerHelper;
  @Mock
  private ConsortiaConfigurationService configurationService;

  @Test
  void shouldReturnCentralTenantId() {
    MessageHeaders messageHeaders = getMessageHeaders();
    when(configurationService.getCentralTenantId(BaseIT.TENANT)).thenReturn(BaseIT.TENANT);
    var actual = eventListenerHelper.getCentralTenantByIdByHeader(messageHeaders);
    Assertions.assertEquals(BaseIT.TENANT, actual);
  }

  @Test
  void shouldThrowError() {
    MessageHeaders messageHeaders = getMessageHeaders();
    when(configurationService.getCentralTenantId(BaseIT.TENANT))
      .thenThrow(new InvalidDataAccessResourceUsageException("Couldn't save object to db"));
    var actual = eventListenerHelper.getCentralTenantByIdByHeader(messageHeaders);
    assertNull(actual);
  }

  private MessageHeaders getMessageHeaders() {
    Map<String, Object> header = new HashMap<>();
    header.put(XOkapiHeaders.TENANT, BaseIT.TENANT.getBytes());
    return new MessageHeaders(header);
  }
}
