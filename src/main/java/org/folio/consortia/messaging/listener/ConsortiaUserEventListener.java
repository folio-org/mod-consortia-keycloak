package org.folio.consortia.messaging.listener;

import org.apache.commons.lang3.StringUtils;
import org.folio.consortia.service.UserAffiliationService;
import org.folio.spring.service.SystemUserScopedExecutionService;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.messaging.MessageHeaders;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Log4j2
@Component
@RequiredArgsConstructor
public class ConsortiaUserEventListener {

  public static final String USER_CREATED_LISTENER_ID = "user-created-listener-id";
  public static final String USER_UPDATED_LISTENER_ID = "user-updated-listener-id";
  public static final String USER_DELETED_LISTENER_ID = "user-deleted-listener-id";
  private final UserAffiliationService userAffiliationService;
  private final EventListenerHelper eventListenerHelper;
  private final SystemUserScopedExecutionService systemUserScopedExecutionService;

  @KafkaListener(
    id = USER_CREATED_LISTENER_ID,
    topicPattern = "#{folioKafkaProperties.listener['user-created'].topicPattern}",
    concurrency = "#{folioKafkaProperties.listener['user-created'].concurrency}",
    containerFactory = "kafkaListenerContainerFactory")
  public void handleUserCreating(String data, MessageHeaders messageHeaders) {
    // to create affiliation in central tenant schema
    String centralTenantId = eventListenerHelper.getCentralTenantByIdByHeader(messageHeaders);
    if (StringUtils.isNotBlank(centralTenantId)) {
      systemUserScopedExecutionService.executeAsyncSystemUserScoped(centralTenantId,
        () -> userAffiliationService.createPrimaryUserAffiliation(data));
    }
  }

  @KafkaListener(
    id = USER_UPDATED_LISTENER_ID,
    topicPattern = "#{folioKafkaProperties.listener['user-updated'].topicPattern}",
    concurrency = "#{folioKafkaProperties.listener['user-updated'].concurrency}",
    containerFactory = "kafkaListenerContainerFactory")
  public void handleUserUpdating(String data, MessageHeaders messageHeaders) {
    // to update affiliation in central tenant schema
    String centralTenantId = eventListenerHelper.getCentralTenantByIdByHeader(messageHeaders);
    if (StringUtils.isNotBlank(centralTenantId)) {
      systemUserScopedExecutionService.executeAsyncSystemUserScoped(centralTenantId, () ->
        userAffiliationService.updatePrimaryUserAffiliation(data));
    }
  }

  @KafkaListener(
    id = USER_DELETED_LISTENER_ID,
    topicPattern = "#{folioKafkaProperties.listener['user-deleted'].topicPattern}",
    concurrency = "#{folioKafkaProperties.listener['user-deleted'].concurrency}",
    containerFactory = "kafkaListenerContainerFactory")
  public void handleUserDeleting(String data, MessageHeaders messageHeaders) {
    // to delete affiliation from central tenant schema
    String centralTenantId = eventListenerHelper.getCentralTenantByIdByHeader(messageHeaders);
    if (StringUtils.isNotBlank(centralTenantId)) {
      systemUserScopedExecutionService.executeAsyncSystemUserScoped(centralTenantId, () ->
        userAffiliationService.deletePrimaryUserAffiliation(data));
    }
  }
}
