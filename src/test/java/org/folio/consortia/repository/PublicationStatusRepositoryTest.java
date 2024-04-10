package org.folio.consortia.repository;

import static java.time.temporal.ChronoUnit.MINUTES;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.within;
import static org.folio.consortia.support.EntityUtils.createPublicationStatusEntity;
import static org.folio.consortia.support.TestConstants.USER_ID;
import static org.mockito.Mockito.when;

import java.time.LocalDateTime;
import org.folio.consortia.base.BaseRepositoryTest;
import org.folio.consortia.domain.dto.PublicationStatus;
import org.folio.consortia.domain.entity.PublicationStatusEntity;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

class PublicationStatusRepositoryTest extends BaseRepositoryTest {

  @Autowired
  private PublicationStatusRepository repository;

  @BeforeEach
  void returnTestUserIdFromFolioExecutionContext() {
    when(folioExecutionContext.getUserId()).thenReturn(USER_ID);
  }

  @Test
  void create_positive_updatedAndCreatedFieldsNotNull() {
    var entity = createPublicationStatusEntity(PublicationStatus.COMPLETE);
    var now = LocalDateTime.now();

    var saved = repository.saveAndFlush(entity);

    var stored = entityManager.find(PublicationStatusEntity.class, saved.getId());
    assertThat(stored.getCreatedDate()).isCloseTo(now, within(1, MINUTES));
    assertThat(stored.getCreatedBy()).isEqualTo(USER_ID);
    assertThat(stored.getUpdatedDate()).isCloseTo(now, within(1, MINUTES));
    assertThat(stored.getUpdatedBy()).isEqualTo(USER_ID);
  }
}
