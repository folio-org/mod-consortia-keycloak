package org.folio.consortia.service;

import static org.folio.consortia.support.EntityUtils.createOkapiHeaders;
import static org.folio.consortia.support.EntityUtils.createTenantEntity;
import static org.folio.consortia.utils.InputOutputTestUtils.getMockDataAsString;
import static org.folio.spring.integration.XOkapiHeaders.TENANT;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.UUID;
import org.folio.consortia.config.kafka.KafkaService;
import org.folio.consortia.domain.dto.PrimaryAffiliationEvent;
import org.folio.consortia.domain.entity.UserTenantEntity;
import org.folio.consortia.repository.TenantRepository;
import org.folio.consortia.service.impl.UserAffiliationServiceImpl;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;


class UserAffiliationServiceTest {
  private static final String userCreatedEventSample = getMockDataAsString("mockdata/kafka/create_primary_affiliation_request.json");
  private static final String userUpdatedEventSample = getMockDataAsString("mockdata/kafka/update_primary_affiliation_request.json");
  private static final String patronUserUpdatedEventSample = getMockDataAsString("mockdata/kafka/update_primary_affiliation_request_patron_user.json");
  private static final String userDeletedEventSample = getMockDataAsString("mockdata/kafka/delete_primary_affiliation_request.json");
  @Mock
  private FolioModuleMetadata folioModuleMetadata;
  @InjectMocks
  UserAffiliationServiceImpl userAffiliationService;
  @Mock
  UserTenantService userTenantService;
  @Mock
  TenantService tenantService;
  @Mock
  TenantRepository tenantRepository;
  @Mock
  ConsortiumService consortiumService;
  @Mock
  KafkaService kafkaService;
  @Mock
  KeycloakUsersService keycloakUsersService;
  @Mock
  PrimaryAffiliationService primaryAffiliationService;
  @Mock
  FolioExecutionContext folioExecutionContext;
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
  void primaryAffiliationAddedSuccessfullyTest() {
    var te = createTenantEntity();
    te.setId(TENANT);

    when(tenantService.getByTenantId(anyString())).thenReturn(te);
    doNothing().when(consortiumService).checkConsortiumExistsOrThrow(any());
    mockOkapiHeaders();

    userAffiliationService.createPrimaryUserAffiliation(userCreatedEventSample);

    verify(primaryAffiliationService, times(1)).createPrimaryAffiliation(any(), anyString(), any(), any());

  }

  @Test
  void primaryAffiliationAddedSuccessfullyTestToCentralTenant() {
    var te = createTenantEntity();

    when(tenantService.getByTenantId(anyString())).thenReturn(te);
    doNothing().when(consortiumService).checkConsortiumExistsOrThrow(any());
    mockOkapiHeaders();

    userAffiliationService.createPrimaryUserAffiliation(userCreatedEventSample);

    verify(primaryAffiliationService, times(1)).createPrimaryAffiliation(any(), anyString(), any(), any());
  }

  @Test
  void tenantNotInConsortiaWhenCreatingTest() {
    when(tenantRepository.findById(anyString())).thenReturn(null);
    mockOkapiHeaders();

    userAffiliationService.createPrimaryUserAffiliation(userCreatedEventSample);

    verify(kafkaService, times(0)).send(any(), anyString(), any());
  }

  @Test
  void primaryAffiliationAlreadyExists() {
    var te = createTenantEntity();

    when(tenantService.getByTenantId(anyString())).thenReturn(te);
    when(userTenantService.checkUserIfHasPrimaryAffiliationByUserId(any(), anyString())).thenReturn(true);
    mockOkapiHeaders();

    userAffiliationService.createPrimaryUserAffiliation(userCreatedEventSample);

    verify(kafkaService, times(0)).send(any(), anyString(), any());
  }

  @Test
  void createPrimaryAffiliationNotParsed() {
    userAffiliationService.createPrimaryUserAffiliation("wrong event payload");

    verifyNoInteractions(kafkaService);
  }

  @Test
  void primaryAffiliationSuccessfullyUpdatedTest() {
    UserTenantEntity userTenant = new UserTenantEntity();
    userTenant.setUserId(UUID.fromString("148f7c24-54fc-4d7f-afff-da2dfcd902e3"));
    userTenant.setUsername("TestUser");

    var te = createTenantEntity();

    when(tenantService.getByTenantId(anyString())).thenReturn(te);
    when(userTenantService.checkUserIfHasPrimaryAffiliationByUserId(te.getConsortiumId(), userTenant.getUserId().toString())).thenReturn(true);
    doNothing().when(consortiumService).checkConsortiumExistsOrThrow(any());
    when(userTenantService.getByUserIdAndTenantId(any(), anyString())).thenReturn(userTenant);
    mockOkapiHeaders();

    userAffiliationService.updatePrimaryUserAffiliation(userUpdatedEventSample);

    verify(kafkaService, times(1)).send(any(), anyString(), any());
  }

  @Test
  void updateWhenPrimaryAffiliationNotExists() {
    String userId = UUID.randomUUID().toString();
    String centralTenantId = "diku";
    var te = createTenantEntity();

    when(tenantService.getByTenantId(anyString())).thenReturn(te);
    when(userTenantService.checkUserIfHasPrimaryAffiliationByUserId(te.getConsortiumId(), userId)).thenReturn(false);
    mockOkapiHeaders();

    userAffiliationService.updatePrimaryUserAffiliation(userUpdatedEventSample);

    verify(primaryAffiliationService).createPrimaryAffiliation(eq(te.getConsortiumId()), eq(centralTenantId), eq(te), any(PrimaryAffiliationEvent.class));
    verifyNoInteractions(kafkaService);
  }

  @Test
  void updateWhenChangingUserTypeFromStaffToPatron() {
    UserTenantEntity userTenant = new UserTenantEntity();
    UUID userId = UUID.fromString("148f7c24-54fc-4d7f-afff-da2dfcd902e3");
    userTenant.setUserId(userId);
    userTenant.setUsername("TestUser");
    var te = createTenantEntity();

    when(tenantService.getByTenantId(anyString())).thenReturn(te);
    when(userTenantService.checkUserIfHasPrimaryAffiliationByUserId(te.getConsortiumId(), userId.toString()))
      .thenReturn(true);
    doNothing().when(consortiumService).checkConsortiumExistsOrThrow(any());
    when(userTenantService.deletePrimaryUserTenantAffiliation(any())).thenReturn(true);
    mockOkapiHeaders();

    userAffiliationService.updatePrimaryUserAffiliation(patronUserUpdatedEventSample);

    verify(userTenantService).deletePrimaryUserTenantAffiliation(userId);
    verify(userTenantService).deleteShadowUsers(userId);
    verify(kafkaService, times(1)).send(any(), anyString(), any());
  }

  @Test
  void updatePrimaryAffiliationNotParsed() {
    userAffiliationService.updatePrimaryUserAffiliation("wrong event payload");

    verifyNoInteractions(kafkaService);
  }

  @Test
  void primaryAffiliationSuccessfullyDeletedTest() {
    var te = createTenantEntity();

    when(tenantService.getByTenantId(anyString())).thenReturn(te);
    doNothing().when(consortiumService).checkConsortiumExistsOrThrow(any());
    when(userTenantService.deletePrimaryUserTenantAffiliation(any())).thenReturn(true);
    mockOkapiHeaders();

    userAffiliationService.deletePrimaryUserAffiliation(userDeletedEventSample);

    verify(userTenantService).deletePrimaryUserTenantAffiliation(any());
    verify(userTenantService).deleteShadowUsers(any());
    verify(kafkaService, times(1)).send(any(), anyString(), any());
  }

  @Test
  void deduplicateDeletePrimaryAffiliationTest() {
    var te = createTenantEntity();

    when(tenantService.getByTenantId(anyString())).thenReturn(te);
    doNothing().when(consortiumService).checkConsortiumExistsOrThrow(any());
    when(userTenantService.deletePrimaryUserTenantAffiliation(any())).thenReturn(false);
    mockOkapiHeaders();

    userAffiliationService.deletePrimaryUserAffiliation(userDeletedEventSample);

    verify(userTenantService, never()).deleteShadowUsers(any());
    verifyNoInteractions(kafkaService);
  }

  @Test
  void kafkaMessageFailedWhenDeletingTest() {
    var te = createTenantEntity();

    when(tenantService.getByTenantId(anyString())).thenReturn(te);
    doNothing().when(consortiumService).checkConsortiumExistsOrThrow(any());
    doThrow(new RuntimeException("Unable to send message to Kafka")).when(kafkaService).send(any(), anyString(), any());
    mockOkapiHeaders();
    when(userTenantService.deletePrimaryUserTenantAffiliation(any())).thenReturn(true);

    userAffiliationService.deletePrimaryUserAffiliation(userDeletedEventSample);

    verify(kafkaService, times(1)).send(any(), anyString(), any());
  }

  @Test
  void tenantNotInConsortiaWhenDeletingTest() {
    when(tenantRepository.findById(anyString())).thenReturn(null);
    mockOkapiHeaders();

    userAffiliationService.deletePrimaryUserAffiliation(userDeletedEventSample);

    verify(kafkaService, times(0)).send(any(), anyString(), any());
  }

  @Test
  void deletePrimaryAffiliationNotParsed() {
    userAffiliationService.deletePrimaryUserAffiliation("wrong event payload");

    verifyNoInteractions(kafkaService);
  }

  private void mockOkapiHeaders() {
    when(folioExecutionContext.getTenantId()).thenReturn("diku");
    when(folioExecutionContext.getOkapiHeaders()).thenReturn(createOkapiHeaders());
  }
}
