package org.folio.consortia.service.impl;

import org.folio.consortia.service.LockService;
import org.springframework.stereotype.Service;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class LockServiceImpl implements LockService {
  //identifiers of advisory lock for tenant setup
  private static final String TENANT_SETUP_LOCK_ID_PARAMS = "23082023, 1008025656";
  private static final String TENANT_SETUP_TRANSACTIONAL_LOCK_SQL = String.format("SELECT pg_advisory_xact_lock(%s)",
    TENANT_SETUP_LOCK_ID_PARAMS);

  private static final String SETUP_LOCAL_LOCK_TIMEOUT_SQL = "SET LOCAL lock_timeout = '300000ms'";

  @PersistenceContext
  private final EntityManager entityManager;

  @Override
  public void lockTenantSetupWithinTransaction() {
    log.info("lockTenantSetupWithinTransaction:: attempting to acquire lock");

    entityManager.createNativeQuery(SETUP_LOCAL_LOCK_TIMEOUT_SQL).executeUpdate();
    entityManager.createNativeQuery(TENANT_SETUP_TRANSACTIONAL_LOCK_SQL).getSingleResult();

    log.info("lockTenantSetupTransactional:: lock acquired");
  }
}
