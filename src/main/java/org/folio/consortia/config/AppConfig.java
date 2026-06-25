package org.folio.consortia.config;

import com.fasterxml.jackson.annotation.JsonInclude;
import tools.jackson.databind.DeserializationFeature;
import tools.jackson.databind.cfg.DateTimeFeature;
import org.folio.consortia.domain.converter.ConsortiumConverter;
import org.folio.consortia.domain.converter.TenantEntityToTenantConverter;
import org.folio.consortia.domain.converter.UserTenantConverter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.jackson.autoconfigure.JsonMapperBuilderCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.core.task.TaskExecutor;
import org.springframework.format.FormatterRegistry;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
@EnableAsync
public class AppConfig implements WebMvcConfigurer {

  @Value("${folio.async-executor.core-pool-size:5}")
  private int asyncExecutorCorePoolSize;

  @Value("${folio.async-executor.max-pool-size:10}")
  private int asyncExecutorMaxPoolSize;

  @Value("${folio.async-executor.queue-capacity:500}")
  private int asyncExecutorQueueCapacity;

  @Value("${folio.publication-executor.core-pool-size:5}")
  private int publicationExecutorCorePoolSize;

  @Value("${folio.publication-executor.max-pool-size:10}")
  private int publicationExecutorMaxPoolSize;

  @Value("${folio.publication-executor.queue-capacity:500}")
  private int publicationExecutorQueueCapacity;

  @Value("${folio.get-publication-executor.core-pool-size:5}")
  private int getPublicationExecutorCorePoolSize;

  @Value("${folio.get-publication-executor.max-pool-size:10}")
  private int getPublicationExecutorMaxPoolSize;

  @Value("${folio.get-publication-executor.queue-capacity:500}")
  private int getPublicationExecutorQueueCapacity;

  @Override
  public void addFormatters(FormatterRegistry registry) {
    registry.addConverter(new TenantEntityToTenantConverter());
    registry.addConverter(new UserTenantConverter());
    registry.addConverter(new ConsortiumConverter());
  }

  /**
   * Default executor used by {@code @Async} methods (e.g. Kafka USER_CREATED/UPDATED/DELETED
   * handlers via {@code executeAsyncSystemUserScoped}). Kept small and isolated from long-running
   * publication work so user-affiliation processing is not blocked while a large share runs.
   */
  @Primary
  @Bean("asyncTaskExecutor")
  public TaskExecutor asyncTaskExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(asyncExecutorCorePoolSize);
    executor.setMaxPoolSize(asyncExecutorMaxPoolSize);
    executor.setQueueCapacity(asyncExecutorQueueCapacity);
    executor.setThreadNamePrefix("ConsortiaAsync-");
    executor.initialize();
    return executor;
  }

  /**
   * Dedicated pool for write publication fan-out (POST/PUT/DELETE, one task per tenant).
   * Separated from {@link #asyncTaskExecutor()} so HTTP-bound publication work cannot starve
   * {@code @Async} consumers.
   */
  @Bean("publicationTaskExecutor")
  public TaskExecutor publicationTaskExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(publicationExecutorCorePoolSize);
    executor.setMaxPoolSize(publicationExecutorMaxPoolSize);
    executor.setQueueCapacity(publicationExecutorQueueCapacity);
    executor.setThreadNamePrefix("ConsortiaPublication-");
    executor.initialize();
    return executor;
  }

  /**
   * Dedicated pool for read publication fan-out (GET, one task per tenant). Separated from
   * {@link #publicationTaskExecutor()} so interactive UI reads (e.g. listing settings across
   * member tenants) are not queued behind long-running write shares.
   */
  @Bean("getPublicationTaskExecutor")
  public TaskExecutor getPublicationTaskExecutor() {
    ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
    executor.setCorePoolSize(getPublicationExecutorCorePoolSize);
    executor.setMaxPoolSize(getPublicationExecutorMaxPoolSize);
    executor.setQueueCapacity(getPublicationExecutorQueueCapacity);
    executor.setThreadNamePrefix("ConsortiaGetPublication-");
    executor.initialize();
    return executor;
  }

  @Bean
  public JsonMapperBuilderCustomizer jsonMapperBuilderCustomizer() {
    return builder -> builder
      .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
      .configure(DateTimeFeature.WRITE_DATES_AS_TIMESTAMPS, false)
      .changeDefaultPropertyInclusion(incl -> incl.withValueInclusion(JsonInclude.Include.NON_NULL))
      .changeDefaultPropertyInclusion(incl -> incl.withContentInclusion(JsonInclude.Include.NON_NULL));
  }

  @Bean
  public RestTemplate restTemplate() {
    return new RestTemplate();
  }
}
