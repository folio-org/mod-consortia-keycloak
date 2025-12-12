package org.folio.consortia.config.kafka;

import org.folio.consortia.messaging.domain.ConsortiaInputEventType;
import org.folio.consortia.messaging.domain.ConsortiaOutputEventType;
import org.folio.consortia.messaging.listener.ConsortiaSharingInstanceEventListener;
import org.folio.consortia.messaging.listener.ConsortiaUserEventListener;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;
import org.apache.kafka.clients.admin.NewTopic;
import org.apache.kafka.clients.producer.ProducerRecord;
import org.folio.consortia.config.kafka.properties.FolioKafkaProperties;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.integration.XOkapiHeaders;
import org.folio.spring.tools.kafka.KafkaUtils;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.kafka.config.KafkaListenerEndpointRegistry;
import org.springframework.kafka.config.TopicBuilder;
import org.springframework.kafka.core.KafkaAdmin;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Component
@Log4j2
@RequiredArgsConstructor
public class KafkaService {

  private final KafkaAdmin kafkaAdmin;
  private final BeanFactory beanFactory;
  private final FolioExecutionContext folioExecutionContext;
  private final KafkaListenerEndpointRegistry kafkaListenerEndpointRegistry;
  private final FolioKafkaProperties folioKafkaProperties;
  private final String kafkaEnvId;
  private final KafkaTemplate<String, Object> kafkaTemplate;

  @RequiredArgsConstructor
  @AllArgsConstructor
  @Getter
  public enum Topic {
    USER_CREATED("USER_CREATED"),
    USER_UPDATED("USER_UPDATED"),
    USER_DELETED("USER_DELETED"),
    CONSORTIUM_INSTANCE_SHARING_INIT("CONSORTIUM_INSTANCE_SHARING_INIT"),
    CONSORTIUM_INSTANCE_SHARING_COMPLETE("CONSORTIUM_INSTANCE_SHARING_COMPLETE"),
    CONSORTIUM_PRIMARY_AFFILIATION_CREATED("Default", "CONSORTIUM_PRIMARY_AFFILIATION_CREATED"),
    CONSORTIUM_PRIMARY_AFFILIATION_UPDATED("Default", "CONSORTIUM_PRIMARY_AFFILIATION_UPDATED"),
    CONSORTIUM_PRIMARY_AFFILIATION_DELETED("Default", "CONSORTIUM_PRIMARY_AFFILIATION_DELETED");
    private String nameSpace;
    private final String topicName;
  }

  public void createKafkaTopics() {
    if (folioExecutionContext == null) {
      throw new IllegalStateException("Could be executed only in Folio-request scope");
    }
    var tenantId = folioExecutionContext.getTenantId();
    var topicList = tenantSpecificTopics(tenantId);

    log.info("Creating topics for kafka [topics: {}]", topicList);
    var configurableBeanFactory = (ConfigurableBeanFactory) beanFactory;
    topicList.forEach(newTopic -> {
      var beanName = newTopic.name() + ".topic";
      if (!configurableBeanFactory.containsBean(beanName)) {
        configurableBeanFactory.registerSingleton(beanName, newTopic);
      }
    });
    kafkaAdmin.initialize();
    restartEventListeners();
  }

  /**
   * Restarts kafka event listeners in mod-consortia-keycloak application.
   */
  public void restartEventListeners() {
    restartEventListener(ConsortiaUserEventListener.USER_CREATED_LISTENER_ID);
    restartEventListener(ConsortiaUserEventListener.USER_UPDATED_LISTENER_ID);
    restartEventListener(ConsortiaUserEventListener.USER_DELETED_LISTENER_ID);
    restartEventListener(ConsortiaSharingInstanceEventListener.CONSORTIUM_INSTANCE_SHARING_COMPLETE_LISTENER_ID);
  }

  private void restartEventListener(String listenerId) {
    log.info("Restarting kafka consumer to start listening topics [id: {}]", listenerId);
    var listenerContainer = kafkaListenerEndpointRegistry.getListenerContainer(listenerId);
    if (listenerContainer != null) {
      listenerContainer.stop();
      listenerContainer.start();
    } else {
      log.error("Listener container not found [id: {}]", listenerId);
    }
  }

  private List<NewTopic> tenantSpecificTopics(String tenant) {
    var eventsNameStreamBuilder = Stream.<Enum<?>>builder();
    for (ConsortiaInputEventType consEventType : ConsortiaInputEventType.values()) {
      eventsNameStreamBuilder.add(consEventType);
    }

    for (ConsortiaOutputEventType consEventType : ConsortiaOutputEventType.values()) {
      eventsNameStreamBuilder.add(consEventType);
    }

    return eventsNameStreamBuilder.build()
      .map(Enum::name)
      .map(topic -> getTenantTopicName(topic, tenant))
      .map(this::toKafkaTopic)
      .toList();
  }

  private NewTopic toKafkaTopic(String topic) {
    return TopicBuilder.name(topic)
      .replicas(folioKafkaProperties.getReplicationFactor())
      .partitions(folioKafkaProperties.getNumberOfPartitions())
      .build();
  }

  /**
   * Returns topic name in the format - `{env}.{tenant}.topicName`
   *
   * @param topicName initial topic name as {@link String}
   * @param tenantId  tenant id as {@link String}
   * @return topic name as {@link String} object
   */
  private String getTenantTopicName(String topicName, String tenantId) {
    return KafkaUtils.getTenantTopicNameWithNamespace(topicName, kafkaEnvId, tenantId, "Default");
  }

  public void send(Topic topic, String key, String data) {
    String tenant = folioExecutionContext.getTenantId();
    if (StringUtils.isBlank(tenant)) {
      throw new IllegalStateException("Can't send to Kafka because tenant is blank");
    }
    String tenantTopicName = getTenantTopicName(topic.getTopicName(), tenant);
    log.debug("Sending event with key: {} to topic: {} for tenant: {}", key, tenantTopicName, tenant);
    ProducerRecord<String, Object> producerRecord = createProducerRecord(tenantTopicName, key, data);
    kafkaTemplate.send(producerRecord);
    log.info("Kafka event sent with key: {} to topic: {} for tenant: {}", key, tenantTopicName, tenant);
  }

  private ProducerRecord<String, Object> createProducerRecord(String tenantTopicName, String key, String data) {
    ProducerRecord<String, Object> producerRecord = new ProducerRecord<>(tenantTopicName, key, data);
    producerRecord.headers().add(XOkapiHeaders.TENANT, folioExecutionContext.getTenantId().getBytes(StandardCharsets.UTF_8));
    producerRecord.headers().add(XOkapiHeaders.TOKEN, folioExecutionContext.getToken().getBytes(StandardCharsets.UTF_8));
    producerRecord.headers().add(XOkapiHeaders.URL, folioExecutionContext.getOkapiUrl().getBytes(StandardCharsets.UTF_8));
    if (Objects.nonNull(folioExecutionContext.getUserId())) {
      producerRecord.headers().add(XOkapiHeaders.USER_ID, folioExecutionContext.getUserId().toString().getBytes(StandardCharsets.UTF_8));
    }
    if (Objects.nonNull(folioExecutionContext.getRequestId())) {
      producerRecord.headers().add(XOkapiHeaders.REQUEST_ID, folioExecutionContext.getRequestId().getBytes(StandardCharsets.UTF_8));
    }
    return producerRecord;
  }
}
