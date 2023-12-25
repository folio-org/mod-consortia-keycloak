package org.folio.consortia.messaging.listener;

import static org.folio.consortia.utils.InputOutputTestUtils.getMockDataAsString;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import org.folio.consortia.service.SharingInstanceService;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import org.folio.consortia.support.BaseIT;
import org.folio.spring.integration.XOkapiHeaders;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.jdbc.BadSqlGrammarException;
import org.springframework.messaging.MessageHeaders;

@SpringBootTest
class ConsortiaSharingInstanceEventListenerTest {

  private static final String CONSORTIUM_INSTANCE_SHARING_COMPLETE_EVENT_SAMPLE = getMockDataAsString("mockdata/kafka/consortium_instance_sharing_complete_request.json");

  @InjectMocks
  private ConsortiaSharingInstanceEventListener eventListener;
  @Mock
  private SharingInstanceService sharingInstanceService;
  @Mock
  private EventListenerHelper eventListenerHelper;

  @Test
  void shouldCompleteInstanceSharingWhenConfigurationExists() {
    MessageHeaders messageHeaders = getMessageHeaders();
    when(eventListenerHelper.getCentralTenantByIdByHeader(messageHeaders)).thenReturn(BaseIT.TENANT);
    eventListener.handleConsortiumInstanceSharingCompleting(CONSORTIUM_INSTANCE_SHARING_COMPLETE_EVENT_SAMPLE, messageHeaders);
    verify(sharingInstanceService).completePromotingLocalInstance(anyString());
  }

  @Test
  void shouldThrowErrorWhenBusinessExceptionThrown() {
    MessageHeaders messageHeaders = getMessageHeaders();
    when(eventListenerHelper.getCentralTenantByIdByHeader(messageHeaders)).thenThrow(new RuntimeException("Operation failed"));
    assertThrows(java.lang.RuntimeException.class,
      () -> eventListener.handleConsortiumInstanceSharingCompleting(CONSORTIUM_INSTANCE_SHARING_COMPLETE_EVENT_SAMPLE, messageHeaders));
  }

  @Test
  void shouldNotThrowErrorWhenCouldNotGetCentralTenantId() {
    MessageHeaders messageHeaders = getMessageHeaders();
    when(eventListenerHelper.getCentralTenantByIdByHeader(messageHeaders)).
      thenThrow(new BadSqlGrammarException("table 'consortia_configuration' not found", "", new SQLException()));
    assertThrows(org.springframework.jdbc.BadSqlGrammarException.class,
      () -> eventListener.handleConsortiumInstanceSharingCompleting(CONSORTIUM_INSTANCE_SHARING_COMPLETE_EVENT_SAMPLE, messageHeaders));
    verifyNoInteractions(sharingInstanceService);
  }

  private MessageHeaders getMessageHeaders() {
    Map<String, Object> header = new HashMap<>();
    header.put(XOkapiHeaders.TENANT, BaseIT.TENANT.getBytes());

    return new MessageHeaders(header);
  }
}
