package org.folio.consortia.repository;

import static java.time.temporal.ChronoUnit.MINUTES;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.within;
import static org.folio.consortia.support.EntityUtils.createPublicationStatusEntity;
import static org.folio.consortia.support.TestConstants.USER_ID;
import static org.mockito.Mockito.when;

import java.time.LocalDateTime;
import java.util.UUID;
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

  @Test
  void deleteAllByCreatedDateBefore_positive_removesOnlyRecordsOlderThanCutoff() {
    var expiredEntity = createPublicationStatusEntity(PublicationStatus.COMPLETE);
    var freshEntity = createPublicationStatusEntity(PublicationStatus.COMPLETE);
    entityManager.persistAndFlush(expiredEntity);
    entityManager.persistAndFlush(freshEntity);

    var cutoff = LocalDateTime.now().minusDays(1);
    backdateCreatedDate(expiredEntity.getId(), cutoff.minusDays(1));
    entityManager.clear();

    var deletedCount = repository.deleteAllByCreatedDateBefore(cutoff);
    entityManager.clear();

    assertThat(deletedCount).isEqualTo(1);
    assertThat(repository.findById(expiredEntity.getId())).isEmpty();
    assertThat(repository.findById(freshEntity.getId())).isPresent();
  }

  private void backdateCreatedDate(UUID id, LocalDateTime createdDate) {
    entityManager.getEntityManager()
      .createNativeQuery("UPDATE pc_state SET created_date = ?1 WHERE id = ?2")
      .setParameter(1, createdDate)
      .setParameter(2, id)
      .executeUpdate();
  }
}
