package org.folio.consortia.config;

import org.apache.commons.lang3.StringUtils;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.config.properties.FolioEnvironment;
import org.folio.spring.context.ExecutionContextBuilder;
import org.folio.spring.model.SystemUser;
import org.folio.spring.model.UserToken;
import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;


class FolioExecutionContextBuilderTest {
  private final ExecutionContextBuilder builder =
    new ExecutionContextBuilder(mock(FolioEnvironment.class), mock(FolioModuleMetadata.class));

  @Test
  void canCreateSystemUserContext() {
    UUID userId = UUID.randomUUID();

    var systemUser = SystemUser.builder()
      .userId(userId.toString())
      .okapiUrl("okapi")
      .username("username")
      .tenantId("tenant")
      .token(new UserToken("token", Instant.MAX)).build();

    var context = builder.forSystemUser(systemUser);

    assertThat(context.getTenantId()).isEqualTo("tenant");
    assertThat(context.getToken()).isEqualTo("token");
    assertThat(context.getUserId()).isEqualTo(userId);
    assertThat(context.getOkapiUrl()).isEqualTo("okapi");

    assertThat(context.getAllHeaders()).isNotNull();
    assertThat(context.getOkapiHeaders()).isNotNull();
    assertThat(context.getFolioModuleMetadata()).isNotNull();

    systemUser = SystemUser.builder()
      .userId(userId.toString())
      .okapiUrl("okapi")
      .username("username")
      .tenantId("tenant")
      .token(null).build();

    context = builder.forSystemUser(systemUser);

    assertThat(context.getTenantId()).isEqualTo("tenant");
    assertThat(context.getToken()).isEqualTo("");
    assertThat(context.getUserId()).isEqualTo(userId);
    assertThat(context.getOkapiUrl()).isEqualTo("okapi");

    assertThat(context.getAllHeaders()).isNotNull();
    assertThat(context.getOkapiHeaders()).isNotNull();
    assertThat(context.getFolioModuleMetadata()).isNotNull();
  }
}
