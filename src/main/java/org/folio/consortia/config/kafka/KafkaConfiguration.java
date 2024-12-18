package org.folio.consortia.config.kafka;

import lombok.RequiredArgsConstructor;
import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.apache.kafka.common.serialization.StringSerializer;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.kafka.DefaultKafkaConsumerFactoryCustomizer;
import org.springframework.boot.autoconfigure.kafka.DefaultKafkaProducerFactoryCustomizer;
import org.springframework.boot.autoconfigure.kafka.KafkaProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.config.ConcurrentKafkaListenerContainerFactory;
import org.springframework.kafka.core.ConsumerFactory;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.core.ProducerFactory;
import org.springframework.stereotype.Component;

@Component
@Configuration
@RequiredArgsConstructor
public class KafkaConfiguration {

  private final KafkaProperties kafkaProperties;

  @Bean
  public String kafkaEnvId(@Value("${ENV:folio}") String envId) {
    return envId;
  }
  @Bean
  public <V> ConcurrentKafkaListenerContainerFactory<String, V> kafkaListenerContainerFactory(ConsumerFactory<String, V> consumerFactory) {
    var factory = new ConcurrentKafkaListenerContainerFactory<String, V>();
    factory.setConsumerFactory(consumerFactory);
    if (kafkaProperties.getListener().getAckMode() != null) {
      factory.getContainerProperties().setAckMode(kafkaProperties.getListener().getAckMode());
    }
    return factory;
  }

  @Bean
  public DefaultKafkaConsumerFactoryCustomizer consumerFactoryCustomizer(FolioModuleMetadata folioModuleMetadata) {
    return consumerFactory -> {
      var props = consumerFactory.getConfigurationProperties();
      props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class);
      props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class);
      props.put("folioModuleMetadata", folioModuleMetadata);
      consumerFactory.updateConfigs(props);
    };
  }

  @Bean
  public DefaultKafkaProducerFactoryCustomizer producerFactoryCustomizer(FolioExecutionContext folioExecutionContext) {
    return producerFactory -> {
      var props = producerFactory.getConfigurationProperties();
      props.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class);
      props.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, StringSerializer.class);
      props.put("folioExecutionContext", folioExecutionContext);
      producerFactory.updateConfigs(props);
    };
  }

  @Bean
  public <V> KafkaTemplate<String, V> kafkaTemplate(ProducerFactory<String, V> producerFactory) {
    return new KafkaTemplate<>(producerFactory);
  }
}
