package org.folio.consortia.repository;

import static java.time.temporal.ChronoUnit.MINUTES;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.within;
import static org.folio.consortia.utils.EntityUtils.CENTRAL_TENANT_ID;
import static org.folio.consortia.utils.EntityUtils.createConsortiaConfigurationEntity;

import java.time.LocalDateTime;
import org.folio.consortia.domain.entity.ConsortiaConfigurationEntity;
import org.folio.consortia.support.BaseRepositoryTest;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

class ConsortiaConfigurationRepositoryTest extends BaseRepositoryTest {

  @Autowired
  private ConsortiaConfigurationRepository repository;

  @Test
  void create_positive_updateDateAndCreatedDateNotNull() {
    var entity = createConsortiaConfigurationEntity(CENTRAL_TENANT_ID);
    entity.setId(null);
    var now = LocalDateTime.now();

    var saved = repository.saveAndFlush(entity);

    var stored = entityManager.find(ConsortiaConfigurationEntity.class, saved.getId());
    assertThat(stored.getCreatedDate()).isCloseTo(now, within(1, MINUTES));
    assertThat(stored.getUpdatedDate()).isCloseTo(now, within(1, MINUTES));
  }
}
