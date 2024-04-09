package org.folio.consortia.repository;

import static java.time.temporal.ChronoUnit.MINUTES;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.within;
import static org.folio.consortia.utils.EntityUtils.createPublicationStatusEntity;

import java.time.LocalDateTime;
import org.folio.consortia.domain.dto.PublicationStatus;
import org.folio.consortia.domain.entity.PublicationStatusEntity;
import org.folio.consortia.support.BaseRepositoryTest;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

class PublicationStatusRepositoryTest extends BaseRepositoryTest {

  @Autowired
  private PublicationStatusRepository repository;

  @Test
  void create_positive_updateDateAndCreatedDateNotNull() {
    var entity = createPublicationStatusEntity(PublicationStatus.COMPLETE);
    var now = LocalDateTime.now();

    var saved = repository.saveAndFlush(entity);

    var stored = entityManager.find(PublicationStatusEntity.class, saved.getId());
    assertThat(stored.getCreatedDate()).isCloseTo(now, within(1, MINUTES));
    assertThat(stored.getUpdatedDate()).isCloseTo(now, within(1, MINUTES));
  }
}
