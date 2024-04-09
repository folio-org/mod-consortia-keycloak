package org.folio.consortia.repository;

import static java.time.temporal.ChronoUnit.MINUTES;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.within;
import static org.folio.consortia.utils.EntityUtils.CONSORTIUM_ID;
import static org.folio.consortia.utils.EntityUtils.createConsortiumEntity;

import java.time.LocalDateTime;
import org.folio.consortia.domain.entity.ConsortiumEntity;
import org.folio.consortia.support.BaseRepositoryTest;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

class ConsortiumRepositoryTest extends BaseRepositoryTest {

  @Autowired
  private ConsortiumRepository repository;

  @Test
  void create_positive_updateDateAndCreatedDateNotNull() {
    var entity = createConsortiumEntity(CONSORTIUM_ID.toString(), "Consortium");
    var now = LocalDateTime.now();

    var saved = repository.saveAndFlush(entity);

    var stored = entityManager.find(ConsortiumEntity.class, saved.getId());
    assertThat(stored.getCreatedDate()).isCloseTo(now, within(1, MINUTES));
    assertThat(stored.getUpdatedDate()).isCloseTo(now, within(1, MINUTES));
  }
}
