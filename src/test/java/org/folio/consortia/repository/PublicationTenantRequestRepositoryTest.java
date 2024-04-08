package org.folio.consortia.repository;

import static java.time.temporal.ChronoUnit.MINUTES;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.within;
import static org.folio.consortia.utils.EntityUtils.createPublicationStatusEntity;
import static org.folio.consortia.utils.EntityUtils.createPublicationTenantRequestEntity;

import java.time.LocalDateTime;
import org.apache.commons.lang3.RandomStringUtils;
import org.folio.consortia.domain.dto.PublicationStatus;
import org.folio.consortia.domain.entity.PublicationTenantRequestEntity;
import org.folio.consortia.support.BaseRepositoryTest;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

class PublicationTenantRequestRepositoryTest extends BaseRepositoryTest {

  @Autowired
  private PublicationTenantRequestRepository repository;

  @Test
  void create_positive_updateDateAndCreatedDateNotNull() {
    var statusEntity = createPublicationStatusEntity(PublicationStatus.COMPLETE);
    entityManager.persistAndFlush(statusEntity);

    var entity = createPublicationTenantRequestEntity(statusEntity, "test",
      PublicationStatus.COMPLETE, 0);
    entity.setRequestUrl(RandomStringUtils.randomAlphanumeric(10));
    var now = LocalDateTime.now();

    var saved = repository.saveAndFlush(entity);

    var stored = entityManager.find(PublicationTenantRequestEntity.class, saved.getId());
    assertThat(stored.getCreatedDate()).isCloseTo(now, within(1, MINUTES));
    assertThat(stored.getUpdatedDate()).isCloseTo(now, within(1, MINUTES));
  }
}
