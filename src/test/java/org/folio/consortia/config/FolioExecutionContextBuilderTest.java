package org.folio.consortia.config;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;

import java.time.Instant;
import java.util.UUID;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.config.properties.FolioEnvironment;
import org.folio.spring.context.ExecutionContextBuilder;
import org.folio.spring.model.SystemUser;
import org.folio.spring.model.UserToken;
import org.junit.jupiter.api.Test;

import lombok.val;


class FolioExecutionContextBuilderTest {
  private final ExecutionContextBuilder builder =
    new ExecutionContextBuilder(mock(FolioEnvironment.class), mock(FolioModuleMetadata.class));

  @Test
  void canCreateSystemUserContext() {
    UUID userId = UUID.randomUUID();

    var systemUser1 = SystemUser.builder()
      .userId(userId.toString())
      .okapiUrl("okapi")
      .username("username")
      .tenantId("tenant")
      .token(new UserToken("token", Instant.MAX)).build();

    var context = builder.forSystemUser(systemUser1, () -> systemUser1);

    assertThat(context.getTenantId()).isEqualTo("tenant");
    assertThat(context.getToken()).isEqualTo("token");
    assertThat(context.getUserId()).isEqualTo(userId);
    assertThat(context.getOkapiUrl()).isEqualTo("okapi");

    assertThat(context.getAllHeaders()).isNotNull();
    assertThat(context.getOkapiHeaders()).isNotNull();
    assertThat(context.getFolioModuleMetadata()).isNotNull();

    var systemUser2 = SystemUser.builder()
      .userId(userId.toString())
      .okapiUrl("okapi")
      .username("username")
      .tenantId("tenant")
      .token(null).build();

    context = builder.forSystemUser(systemUser2, () -> systemUser2);

    assertThat(context.getTenantId()).isEqualTo("tenant");
    assertThat(context.getToken()).isEqualTo("");
    assertThat(context.getUserId()).isEqualTo(userId);
    assertThat(context.getOkapiUrl()).isEqualTo("okapi");

    assertThat(context.getAllHeaders()).isNotNull();
    assertThat(context.getOkapiHeaders()).isNotNull();
    assertThat(context.getFolioModuleMetadata()).isNotNull();
  }
}
