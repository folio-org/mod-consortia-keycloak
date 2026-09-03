package org.folio.consortia.repository;

import static java.time.temporal.ChronoUnit.MINUTES;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.within;
import static org.folio.consortia.support.EntityUtils.createPublicationStatusEntity;
import static org.folio.consortia.support.EntityUtils.createPublicationTenantRequestEntity;
import static org.folio.consortia.support.TestConstants.USER_ID;
import static org.mockito.Mockito.when;

import java.time.LocalDateTime;
import java.util.UUID;
import org.apache.commons.lang3.RandomStringUtils;
import org.folio.consortia.base.BaseRepositoryTest;
import org.folio.consortia.domain.dto.PublicationStatus;
import org.folio.consortia.domain.entity.PublicationTenantRequestEntity;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

class PublicationTenantRequestRepositoryTest extends BaseRepositoryTest {

  @Autowired
  private PublicationTenantRequestRepository repository;

  @BeforeEach
  void returnTestUserIdFromFolioExecutionContext() {
    when(folioExecutionContext.getUserId()).thenReturn(USER_ID);
  }

  @Test
  void create_positive_updatedAndCreatedFieldsNotNull() {
    var statusEntity = createPublicationStatusEntity(PublicationStatus.COMPLETE);
    entityManager.persistAndFlush(statusEntity);

    var entity = createPublicationTenantRequestEntity(statusEntity, "test",
      PublicationStatus.COMPLETE, 0);
    entity.setRequestUrl(RandomStringUtils.randomAlphanumeric(10));
    var now = LocalDateTime.now();

    var saved = repository.saveAndFlush(entity);

    var stored = entityManager.find(PublicationTenantRequestEntity.class, saved.getId());
    assertThat(stored.getCreatedDate()).isCloseTo(now, within(1, MINUTES));
    assertThat(stored.getCreatedBy()).isEqualTo(USER_ID);
    assertThat(stored.getUpdatedDate()).isCloseTo(now, within(1, MINUTES));
    assertThat(stored.getUpdatedBy()).isEqualTo(USER_ID);
  }

  @Test
  void deleteAllByCreatedDateBefore_positive_removesOnlyRecordsOlderThanCutoff() {
    var statusEntity = createPublicationStatusEntity(PublicationStatus.COMPLETE);
    entityManager.persistAndFlush(statusEntity);

    var expiredEntity = createPublicationTenantRequestEntity(statusEntity, "expired-tenant",
      PublicationStatus.COMPLETE, 0);
    expiredEntity.setRequestUrl(RandomStringUtils.randomAlphanumeric(10));
    var freshEntity = createPublicationTenantRequestEntity(statusEntity, "fresh-tenant",
      PublicationStatus.COMPLETE, 0);
    freshEntity.setRequestUrl(RandomStringUtils.randomAlphanumeric(10));
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
      .createNativeQuery("UPDATE pc_tenant_request SET created_date = ?1 WHERE id = ?2")
      .setParameter(1, createdDate)
      .setParameter(2, id)
      .executeUpdate();
  }
}
