package org.folio.consortia.service;

import java.sql.ResultSet;
import java.util.Map;
import liquibase.exception.LiquibaseException;
import liquibase.exception.UnexpectedLiquibaseException;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.BooleanUtils;
import org.folio.consortia.config.kafka.KafkaService;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.exception.TenantUpgradeException;
import org.folio.spring.liquibase.FolioSpringLiquibase;
import org.folio.spring.service.TenantService;
import org.folio.tenant.domain.dto.TenantAttributes;
import org.springframework.context.annotation.Primary;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

@Log4j2
@Service
@Primary
public class FolioTenantService extends TenantService {

  private static final String EXIST_SQL =
    "SELECT EXISTS(SELECT 1 FROM INFORMATION_SCHEMA.SCHEMATA WHERE SCHEMA_NAME = ?)";
  private static final String TENANT_NAME_PARAMETER = "tenantname";

  private final KafkaService kafkaService;
  private final FolioExecutionContext folioExecutionContext;

  public FolioTenantService(JdbcTemplate jdbcTemplate,
                            KafkaService kafkaService,
                            FolioExecutionContext context,
                            FolioSpringLiquibase folioSpringLiquibase,
                            FolioExecutionContext folioExecutionContext) {
    super(jdbcTemplate, context, folioSpringLiquibase);
    this.kafkaService = kafkaService;
    this.folioExecutionContext = folioExecutionContext;
  }

  /*
   * Because of the liquibase.Scope implementation for the SpringLiquibase it is not possible to run several
   * SpringLiquibase executions simultaneously. That is why this method must be synchronized.
   */
  @Override
  public synchronized void createOrUpdateTenant(TenantAttributes tenantAttributes) {
    if (folioSpringLiquibase != null) {
      var params = Map.of(TENANT_NAME_PARAMETER, folioExecutionContext.getTenantId());
      folioSpringLiquibase.setChangeLogParameters(params);
      log.info("Set ChangeLog parameters: {}", params);

      folioSpringLiquibase.setDefaultSchema(getSchemaName());
      log.info("About to start Liquibase update for tenant [{}]", context.getTenantId());

      try {
        folioSpringLiquibase.performLiquibaseUpdate();
      } catch (LiquibaseException | UnexpectedLiquibaseException e) {
        throw new TenantUpgradeException(e);
      }
      log.info("Liquibase update for tenant [{}] executed successfully", context.getTenantId());
    }

    afterTenantUpdate(tenantAttributes);
  }

  @Override
  protected void afterTenantUpdate(TenantAttributes tenantAttributes) {
    try {
      kafkaService.createKafkaTopics();
    } catch (Exception e) {
      log.error(e.getMessage(), e);
      throw e;
    }
  }

  /**
   * Implemented by HSQLDB way. Check if the tenant exists (by way of its database schema)
   *
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
}
