package org.folio.consortia.messaging.listener;

import static org.folio.consortia.utils.TenantContextUtils.createFolioExecutionContext;
import static org.folio.consortia.utils.TenantContextUtils.runInFolioContext;

import org.apache.commons.lang3.StringUtils;
import org.folio.consortia.service.SharingInstanceService;
import org.folio.spring.FolioModuleMetadata;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.messaging.MessageHeaders;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Log4j2
@Component
@RequiredArgsConstructor
public class ConsortiaSharingInstanceEventListener {

  public static final String CONSORTIUM_INSTANCE_SHARING_COMPLETE_LISTENER_ID = "consortium-instance-sharing-complete-listener-id";
  private final SharingInstanceService sharingInstanceService;
  private final FolioModuleMetadata folioMetadata;
  private final EventListenerHelper eventListenerHelper;

  @KafkaListener(
    id = CONSORTIUM_INSTANCE_SHARING_COMPLETE_LISTENER_ID,
    topicPattern = "#{folioKafkaProperties.listener['consortium-instance-sharing-complete'].topicPattern}",
    concurrency = "#{folioKafkaProperties.listener['consortium-instance-sharing-complete'].concurrency}",
    containerFactory = "kafkaListenerContainerFactory")
  public void handleConsortiumInstanceSharingCompleting(String data, MessageHeaders messageHeaders) {
    String centralTenantId = eventListenerHelper.getCentralTenantByIdByHeader(messageHeaders);
    if (StringUtils.isNotBlank(centralTenantId)) {
      runInFolioContext(createFolioExecutionContext(messageHeaders, folioMetadata, centralTenantId),
        () -> sharingInstanceService.completePromotingLocalInstance(data));
    }
  }
}
