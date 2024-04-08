package org.folio.consortia.support.extension.impl;

import org.junit.jupiter.api.extension.AfterAllCallback;
import org.junit.jupiter.api.extension.BeforeAllCallback;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.testcontainers.containers.PostgreSQLContainer;

public class PostgresContainerExtension implements BeforeAllCallback, AfterAllCallback {
  private static final String SPRING_DATASOURCE_URL = "spring.datasource.url";
  private static final String SPRING_DATASOURCE_USERNAME = "spring.datasource.username";
  private static final String SPRING_DATASOURCE_PASSWORD = "spring.datasource.password";

  @SuppressWarnings("resource")
  private static final PostgreSQLContainer<?> CONTAINER = new PostgreSQLContainer<>("postgres:12-alpine")
    .withDatabaseName("okapi_modules")
    .withUsername("folio_admin")
    .withPassword("folio_admin");

  @Override
  public void beforeAll(ExtensionContext context) {
    if (!CONTAINER.isRunning()) {
      CONTAINER.start();
    }

    System.setProperty(SPRING_DATASOURCE_URL, CONTAINER.getJdbcUrl());
    System.setProperty(SPRING_DATASOURCE_USERNAME, CONTAINER.getUsername());
    System.setProperty(SPRING_DATASOURCE_PASSWORD, CONTAINER.getPassword());
  }

  @Override
  public void afterAll(ExtensionContext context) {
    System.clearProperty(SPRING_DATASOURCE_URL);
    System.clearProperty(SPRING_DATASOURCE_USERNAME);
    System.clearProperty(SPRING_DATASOURCE_PASSWORD);
  }
}
