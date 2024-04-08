package org.folio.consortia.repository;

import static java.time.temporal.ChronoUnit.MINUTES;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.within;
import static org.folio.consortia.utils.EntityUtils.CENTRAL_TENANT_ID;
import static org.folio.consortia.utils.EntityUtils.CONSORTIUM_ID;
import static org.folio.consortia.utils.EntityUtils.createConsortiumEntity;
import static org.folio.consortia.utils.EntityUtils.createTenantDetailsEntity;

import java.time.LocalDateTime;
import org.folio.consortia.domain.entity.TenantDetailsEntity;
import org.folio.consortia.support.BaseRepositoryTest;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

class TenantDetailsRepositoryTest extends BaseRepositoryTest {

  @Autowired
  private TenantDetailsRepository repository;

  @Test
  void create_positive_updateDateAndCreatedDateNotNull() {
    var consortium = createConsortiumEntity(CONSORTIUM_ID.toString(), CENTRAL_TENANT_ID);
    entityManager.persistAndFlush(consortium);

    var entity = createTenantDetailsEntity();
    entity.setConsortiumId(consortium.getId());
    var now = LocalDateTime.now();

    var saved = repository.saveAndFlush(entity);

    var stored = entityManager.find(TenantDetailsEntity.class, saved.getId());
    assertThat(stored.getCreatedDate()).isCloseTo(now, within(1, MINUTES));
    assertThat(stored.getUpdatedDate()).isCloseTo(now, within(1, MINUTES));
  }
}
