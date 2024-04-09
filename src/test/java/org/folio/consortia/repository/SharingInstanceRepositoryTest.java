package org.folio.consortia.repository;

import static java.time.temporal.ChronoUnit.MINUTES;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.within;
import static org.folio.consortia.utils.EntityUtils.createSharingInstanceEntity;

import java.time.LocalDateTime;
import java.util.UUID;
import org.folio.consortia.domain.dto.Status;
import org.folio.consortia.domain.entity.SharingInstanceEntity;
import org.folio.consortia.support.BaseRepositoryTest;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

class SharingInstanceRepositoryTest extends BaseRepositoryTest {

  @Autowired
  private SharingInstanceRepository repository;

  @Test
  void create_positive_updateDateAndCreatedDateNotNull() {
    var entity = createSharingInstanceEntity(UUID.randomUUID(), UUID.randomUUID(), "test1", "test2");
    entity.setStatus(Status.COMPLETE);
    var now = LocalDateTime.now();

    var saved = repository.saveAndFlush(entity);

    var stored = entityManager.find(SharingInstanceEntity.class, saved.getId());
    assertThat(stored.getCreatedDate()).isCloseTo(now, within(1, MINUTES));
    assertThat(stored.getUpdatedDate()).isCloseTo(now, within(1, MINUTES));
  }
}
