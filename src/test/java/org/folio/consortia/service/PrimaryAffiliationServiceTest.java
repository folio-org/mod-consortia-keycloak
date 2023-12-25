package org.folio.consortia.service;

import org.folio.consortia.config.kafka.KafkaService;
import org.folio.consortia.domain.entity.TenantEntity;
import org.folio.consortia.service.impl.PrimaryAffiliationServiceImpl;
import org.folio.consortia.domain.dto.PrimaryAffiliationEvent;
import org.folio.consortia.domain.dto.UserTenant;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.UUID;

import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

class PrimaryAffiliationServiceTest {
  private static final UUID CONSORTIUM_ID = UUID.randomUUID();
  private static final String CENTRAL_TENANT_ID = "consortium";
  private static final String MEMBER_TENANT_ID = "university";
  private static final UUID USER_ID = UUID.randomUUID();
  private static final String USERNAME = "username";
  @Mock
  private UserTenantService userTenantService;
  @Mock
  private KafkaService kafkaService;
  @InjectMocks
  private PrimaryAffiliationServiceImpl primaryAffiliationService;
  AutoCloseable mockitoMocks;

  @BeforeEach
  public void beforeEach() {
    mockitoMocks = MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void afterEach() throws Exception {
    mockitoMocks.close();
  }

  @Test
  void testRequiresNewTransactionForCentralTenant() {
    TenantEntity tenantEntity = new TenantEntity();
    tenantEntity.setId(CENTRAL_TENANT_ID);
    PrimaryAffiliationEvent event = getPrimaryAffiliationEvent();

    primaryAffiliationService.createPrimaryAffiliationInNewTransaction(CONSORTIUM_ID, CENTRAL_TENANT_ID, tenantEntity, event);

    verify(userTenantService).createPrimaryUserTenantAffiliation(CONSORTIUM_ID, tenantEntity, USER_ID.toString(), USERNAME);
    verify(userTenantService, never()).save(eq(CONSORTIUM_ID), any(UserTenant.class), eq(true));
    verify(kafkaService).send(eq(KafkaService.Topic.CONSORTIUM_PRIMARY_AFFILIATION_CREATED), eq(event.getUserId().toString()), anyString());
  }

  @Test
  void testRequiresNewTransactionForMemberTenant() {
    TenantEntity tenantEntity = new TenantEntity();
    tenantEntity.setId(MEMBER_TENANT_ID);
    PrimaryAffiliationEvent event = getPrimaryAffiliationEvent();

    primaryAffiliationService.createPrimaryAffiliationInNewTransaction(CONSORTIUM_ID, CENTRAL_TENANT_ID, tenantEntity, event);

    verify(userTenantService).createPrimaryUserTenantAffiliation(CONSORTIUM_ID, tenantEntity, USER_ID.toString(), USERNAME);
    verify(userTenantService).save(eq(CONSORTIUM_ID), any(UserTenant.class), eq(true));
    verify(kafkaService).send(eq(KafkaService.Topic.CONSORTIUM_PRIMARY_AFFILIATION_CREATED), eq(event.getUserId().toString()), anyString());
  }

  @Test
  void testSupportTransactionForCentralTenant() {
    TenantEntity tenantEntity = new TenantEntity();
    tenantEntity.setId(CENTRAL_TENANT_ID);
    PrimaryAffiliationEvent event = getPrimaryAffiliationEvent();

    primaryAffiliationService.createPrimaryAffiliation(CONSORTIUM_ID, CENTRAL_TENANT_ID, tenantEntity, event);

    verify(userTenantService).createPrimaryUserTenantAffiliation(CONSORTIUM_ID, tenantEntity, USER_ID.toString(), USERNAME);
    verify(userTenantService, never()).save(eq(CONSORTIUM_ID), any(UserTenant.class), eq(true));
    verify(kafkaService).send(eq(KafkaService.Topic.CONSORTIUM_PRIMARY_AFFILIATION_CREATED), eq(event.getUserId().toString()), anyString());
  }

  @Test
  void testSupportTransactionForMemberTenant() {
    TenantEntity tenantEntity = new TenantEntity();
    tenantEntity.setId(MEMBER_TENANT_ID);
    PrimaryAffiliationEvent event = getPrimaryAffiliationEvent();

    primaryAffiliationService.createPrimaryAffiliation(CONSORTIUM_ID, CENTRAL_TENANT_ID, tenantEntity, event);

    verify(userTenantService).createPrimaryUserTenantAffiliation(CONSORTIUM_ID, tenantEntity, USER_ID.toString(), USERNAME);
    verify(userTenantService).save(eq(CONSORTIUM_ID), any(UserTenant.class), eq(true));
    verify(kafkaService).send(eq(KafkaService.Topic.CONSORTIUM_PRIMARY_AFFILIATION_CREATED), eq(event.getUserId().toString()), anyString());
  }

  private PrimaryAffiliationEvent getPrimaryAffiliationEvent() {
    PrimaryAffiliationEvent event = new PrimaryAffiliationEvent();
    event.setUserId(USER_ID);
    event.setUsername(USERNAME);
    return event;
  }
}
