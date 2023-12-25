package org.folio.consortia.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.folio.consortia.service.impl.LockServiceImpl;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.batch.BatchAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;

import jakarta.persistence.EntityManager;
import jakarta.persistence.Query;

@SpringBootTest
@EnableAutoConfiguration(exclude = BatchAutoConfiguration.class)
class LockServiceTest {
  @InjectMocks
  private LockServiceImpl lockService;

  @Mock
  private EntityManager entityManager;
  @Mock
  private Query query;

  @Test
  void shouldLockWithinTransaction() {
    when(entityManager.createNativeQuery(any())).thenReturn(query);

    lockService.lockTenantSetupWithinTransaction();

    verify(entityManager).createNativeQuery("SELECT pg_advisory_xact_lock(23082023, 1008025656)");
    verify(entityManager).createNativeQuery("SET LOCAL lock_timeout = '300000ms'");
  }
}
