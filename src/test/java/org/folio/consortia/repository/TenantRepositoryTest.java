package org.folio.consortia.repository;

import static java.time.temporal.ChronoUnit.MINUTES;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.within;
import static org.folio.consortia.support.EntityUtils.createConsortiumEntity;
import static org.folio.consortia.support.EntityUtils.createTenantEntity;
import static org.folio.consortia.support.TestConstants.CENTRAL_TENANT_ID;
import static org.folio.consortia.support.TestConstants.CONSORTIUM_ID;
import static org.folio.consortia.support.TestConstants.USER_ID;
import static org.mockito.Mockito.when;

import java.time.LocalDateTime;
import org.folio.consortia.base.BaseRepositoryTest;
import org.folio.consortia.domain.entity.TenantEntity;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

class TenantRepositoryTest extends BaseRepositoryTest {

  @Autowired
  private TenantRepository repository;

  @BeforeEach
  void returnTestUserIdFromFolioExecutionContext() {
    when(folioExecutionContext.getUserId()).thenReturn(USER_ID);
  }

  @Test
  void create_positive_updatedAndCreatedFieldsNotNull() {
    var consortium = createConsortiumEntity(CONSORTIUM_ID.toString(), CENTRAL_TENANT_ID);
    entityManager.persistAndFlush(consortium);

    var entity = createTenantEntity();
    entity.setConsortiumId(consortium.getId());
    var now = LocalDateTime.now();

    var saved = repository.saveAndFlush(entity);

    var stored = entityManager.find(TenantEntity.class, saved.getId());
    assertThat(stored.getCreatedDate()).isCloseTo(now, within(1, MINUTES));
    assertThat(stored.getCreatedBy()).isEqualTo(USER_ID);
    assertThat(stored.getUpdatedDate()).isCloseTo(now, within(1, MINUTES));
    assertThat(stored.getUpdatedBy()).isEqualTo(USER_ID);
  }
}
