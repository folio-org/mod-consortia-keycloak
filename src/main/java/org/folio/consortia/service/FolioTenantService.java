package org.folio.consortia.service;

import java.sql.ResultSet;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.folio.consortia.config.kafka.KafkaService;
import org.folio.consortia.config.property.CustomFieldsRetryProperties;
import org.folio.consortia.domain.dto.CustomField;
import org.folio.consortia.domain.dto.CustomFieldType;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.liquibase.FolioSpringLiquibase;
import org.folio.spring.service.PrepareSystemUserService;
import org.folio.spring.service.SystemUserScopedExecutionService;
import org.folio.spring.service.TenantService;
import org.folio.tenant.domain.dto.TenantAttributes;
import org.springframework.context.annotation.Primary;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.retry.backoff.FixedBackOffPolicy;
import org.springframework.retry.policy.SimpleRetryPolicy;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.stereotype.Service;

@Log4j2
@Service
@Primary
public class FolioTenantService extends TenantService {

  private static final String EXIST_SQL = "SELECT EXISTS(SELECT 1 FROM INFORMATION_SCHEMA.SCHEMATA WHERE SCHEMA_NAME = ?)";

  private final KafkaService kafkaService;
  private final CustomFieldService customFieldService;
  private final FolioExecutionContext folioExecutionContext;
  private final PrepareSystemUserService prepareSystemUserService;
  private final SystemUserScopedExecutionService systemUserScopedExecutionService;
  private final CustomFieldsRetryProperties customFieldsRetryProperties;

  private static final String ORIGINAL_TENANT_ID_NAME = "originalTenantId";
  private static final CustomField ORIGINAL_TENANT_ID_CUSTOM_FIELD = CustomField.builder()
    .name(ORIGINAL_TENANT_ID_NAME)
    .entityType("user")
    .helpText("Id of tenant where user created originally")
    .customFieldType(CustomFieldType.TEXTBOX_LONG)
    .visible(false)
    .build();

  public FolioTenantService(JdbcTemplate jdbcTemplate,
                            KafkaService kafkaService,
                            FolioExecutionContext context,
                            FolioSpringLiquibase folioSpringLiquibase,
                            CustomFieldService customFieldService,
                            FolioExecutionContext folioExecutionContext,
                            PrepareSystemUserService prepareSystemUserService,
                            SystemUserScopedExecutionService systemUserScopedExecutionService,
                            CustomFieldsRetryProperties customFieldsRetryProperties) {
    super(jdbcTemplate, context, folioSpringLiquibase);
    this.kafkaService = kafkaService;
    this.customFieldService = customFieldService;
    this.folioExecutionContext = folioExecutionContext;
    this.prepareSystemUserService = prepareSystemUserService;
    this.systemUserScopedExecutionService = systemUserScopedExecutionService;
    this.customFieldsRetryProperties = customFieldsRetryProperties;
  }

  @Override
  protected void afterTenantUpdate(TenantAttributes tenantAttributes) {
    try {
      prepareSystemUserService.setupSystemUser();
      kafkaService.createKafkaTopics();
      createOriginalTenantIdCustomField();
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      throw e;
    }
  }

  /**
   * Implemented by HSQLDB way
   * Check if the tenant exists (by way of its database schema)
   * @return if the tenant's database schema exists
   */
  @Override
  protected boolean tenantExists() {
    return BooleanUtils.isTrue(
      jdbcTemplate.query(EXIST_SQL,
        (ResultSet resultSet) -> resultSet.next() && resultSet.getBoolean(1),
        getSchemaName()
      )
    );
  }

  private void createOriginalTenantIdCustomField() {
    systemUserScopedExecutionService.executeAsyncSystemUserScoped(folioExecutionContext.getTenantId(), () -> {
      if (ObjectUtils.isNotEmpty(customFieldService.getCustomFieldByName(ORIGINAL_TENANT_ID_NAME))) {
        log.info("createOriginalTenantIdCustomField:: custom-field already available in tenant {} with name {}", folioExecutionContext.getTenantId(), ORIGINAL_TENANT_ID_NAME);
      } else {
        createCustomFieldsWithRetry();
      }
    });
  }

  private void createCustomFieldsWithRetry() {
    var retryTemplate = new RetryTemplate();

    var fixedBackOffPolicy = new FixedBackOffPolicy();
    fixedBackOffPolicy.setBackOffPeriod(customFieldsRetryProperties.getBackoffDelay());
    retryTemplate.setBackOffPolicy(fixedBackOffPolicy);

    var retryPolicy = new SimpleRetryPolicy();
    retryPolicy.setMaxAttempts(customFieldsRetryProperties.getMaxAttempts());
    retryTemplate.setRetryPolicy(retryPolicy);

    retryTemplate.execute(arg -> {
      customFieldService.createCustomField(ORIGINAL_TENANT_ID_CUSTOM_FIELD);
      return null;
    });
  }
}
