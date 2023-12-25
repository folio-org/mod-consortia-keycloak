package org.folio.consortia.config.kafka.properties;

import java.util.Map;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import lombok.Data;

@Data
@Component
@ConfigurationProperties("folio.kafka")
public class FolioKafkaProperties {

  private int numberOfPartitions;

  private int replicationFactor;

  private Map<String, KafkaListenerProperties> listener;

  @Data
  public static class KafkaListenerProperties {

    private String topicPattern;

    private Integer concurrency = 5;

    private String groupId;
  }
}
