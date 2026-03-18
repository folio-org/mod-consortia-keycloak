package org.folio.consortia.service.impl;

import static org.folio.consortia.support.EntityUtils.createTenantEntity;
import static org.folio.consortia.support.EntityUtils.getFolioExecutionContext;
import static org.folio.consortia.utils.InputOutputTestUtils.getMockDataAsString;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import org.folio.consortia.domain.entity.TenantEntity;
import org.folio.consortia.domain.entity.UserTenantEntity;
import org.folio.consortia.repository.TenantRepository;
import org.folio.consortia.repository.UserTenantRepository;
import org.folio.consortia.service.ConsortiaConfigurationService;
import org.folio.consortia.service.LockService;
import org.folio.consortia.service.PrimaryAffiliationService;
import org.folio.consortia.service.TenantService;
import org.folio.consortia.service.UserService;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.folio.consortia.domain.dto.SyncUser;
import org.folio.consortia.domain.dto.TenantDetails.SetupStatusEnum;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.dto.UserCollection;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.batch.autoconfigure.BatchAutoConfiguration;
import org.springframework.boot.persistence.autoconfigure.EntityScan;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.task.AsyncTaskExecutor;
import org.springframework.core.task.SimpleAsyncTaskExecutor;
import org.springframework.core.task.TaskExecutor;
import org.springframework.dao.DataAccessResourceFailureException;

import tools.jackson.databind.ObjectMapper;

import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.context.bean.override.mockito.MockitoSpyBean;
import org.springframework.web.client.HttpClientErrorException;

@SpringBootTest
@EnableAutoConfiguration(exclude = BatchAutoConfiguration.class)
@EntityScan(basePackageClasses = UserTenantEntity.class)
class SyncPrimaryAffiliationServiceImplTest {

  @MockitoBean
  private TenantRepository tenantRepository;
  @MockitoBean
  private UserTenantRepository userTenantRepository;
  @MockitoBean
  private TenantService tenantService;
  @MockitoBean
  private UserService userService;
  @MockitoBean
  private PrimaryAffiliationService primaryAffiliationService;
  @MockitoBean
  private ConsortiaConfigurationService consortiaConfigurationService;
  @MockitoBean
  private LockService lockService;
  @MockitoSpyBean
  private TaskExecutor asyncTaskExecutor = new SimpleAsyncTaskExecutor();

  private SyncPrimaryAffiliationServiceImpl syncPrimaryAffiliationService;
  private CreatePrimaryAffiliationServiceImpl createPrimaryAffiliationService;

  @BeforeEach
  void setUp() {
    createPrimaryAffiliationService = spy(new CreatePrimaryAffiliationServiceImpl(tenantService, userTenantRepository,
      lockService, primaryAffiliationService));
    syncPrimaryAffiliationService = spy(new SyncPrimaryAffiliationServiceImpl(userService, tenantService,
      createPrimaryAffiliationService, getFolioExecutionContext(), (AsyncTaskExecutor) asyncTaskExecutor));
  }

  @Test
  void createPrimaryUserAffiliationsWhenCentralTenantSaving() {
    var consortiumId = UUID.randomUUID();
    var tenantId = "ABC1";
    var centralTenantId = "diku";
    TenantEntity tenantEntity1 = createTenantEntity(tenantId, "TestName1");
    tenantEntity1.setConsortiumId(consortiumId);

    var userCollectionString = getMockDataAsString("mockdata/user_collection.json");
    List<User> userCollection = new ObjectMapper().readValue(userCollectionString, UserCollection.class).getUsers();

    var syncUser = new SyncUser().id(UUID.randomUUID()
        .toString())
      .username("test_user");

    // stub collection of 2 users
    when(tenantService.getByTenantId(anyString())).thenReturn(tenantEntity1);
    when(userTenantRepository.findByUserIdAndIsPrimaryTrue(any())).thenReturn(Optional.empty());
    when(tenantRepository.findById(anyString())).thenReturn(Optional.of(tenantEntity1));
    when(userService.getPrimaryUsersToLink()).thenReturn(userCollection);
    when(consortiaConfigurationService.getCentralTenantId(anyString())).thenReturn(tenantId);

    createPrimaryAffiliationService.createPrimaryUserAffiliations(consortiumId, centralTenantId, tenantId, List.of(syncUser));

    verify(primaryAffiliationService).createPrimaryAffiliationInNewTransaction(any(), anyString(), any(), any());
    verify(tenantService).updateTenantSetupStatus(tenantId, centralTenantId, SetupStatusEnum.COMPLETED);
    verify(lockService).lockTenantSetupWithinTransaction();
  }
  @Test
  void createPrimaryUserAffiliationsWhenLocalTenantSaving() {
    var consortiumId = UUID.randomUUID();
    var tenantId = "ABC1";
    var centralTenantId = "diku";
    TenantEntity tenantEntity1 = createTenantEntity(tenantId, "TestName1");
    tenantEntity1.setConsortiumId(consortiumId);

    var userCollectionString = getMockDataAsString("mockdata/user_collection.json");
    List<User> userCollection = new ObjectMapper().readValue(userCollectionString, UserCollection.class).getUsers();

    var syncUser = new SyncUser().id(UUID.randomUUID()
        .toString())
      .username("test_user");

    // stub collection of 2 users
    when(tenantService.getByTenantId(anyString())).thenReturn(tenantEntity1);
    when(userTenantRepository.findByUserIdAndIsPrimaryTrue(any())).thenReturn(Optional.empty());
    when(tenantRepository.findById(anyString())).thenReturn(Optional.of(tenantEntity1));
    when(userService.getPrimaryUsersToLink()).thenReturn(userCollection);
    when(consortiaConfigurationService.getCentralTenantId(anyString())).thenReturn(centralTenantId);

    createPrimaryAffiliationService.createPrimaryUserAffiliations(consortiumId, centralTenantId, tenantId, List.of(syncUser));

    verify(primaryAffiliationService).createPrimaryAffiliationInNewTransaction(any(), anyString(), any(), any());
    verify(tenantService).updateTenantSetupStatus(tenantId, centralTenantId, SetupStatusEnum.COMPLETED);
    verify(lockService).lockTenantSetupWithinTransaction();
  }

  @Test
  void syncPrimaryUserAffiliationsWhenTenantSaving() {
    var consortiumId = UUID.randomUUID();
    var tenantId = "ABC1";
    var centralTenantId = "diku";
    TenantEntity tenantEntity1 = createTenantEntity(tenantId, "TestName1");
    tenantEntity1.setConsortiumId(consortiumId);

    var userCollectionString = getMockDataAsString("mockdata/user_collection.json");
    List<User> userCollection = new ObjectMapper().readValue(userCollectionString, UserCollection.class).getUsers();

    var syncUsers = userCollection.stream()
      .map(user -> new SyncUser()
        .id(user.getId())
        .username(user.getUsername())
        .email(user.getPersonal().getEmail())
        .phoneNumber(user.getPersonal().getPhone())
        .mobilePhoneNumber(user.getPersonal().getMobilePhone())
        .externalSystemId(user.getExternalSystemId())
        .barcode(user.getBarcode()))
      .toList();

    doNothing().when(tenantService).updateTenantSetupStatus(tenantId, centralTenantId, SetupStatusEnum.COMPLETED);
    // stub collection of 2 users
    when(userService.getPrimaryUsersToLink()).thenReturn(userCollection);
    // stub userTenantRepository to return record for each user to skip affiliation creation
    when(userTenantRepository.findByUserIdAndIsPrimaryTrue(any(UUID.class))).thenReturn(Optional.of(new UserTenantEntity()));

    syncPrimaryAffiliationService.syncPrimaryAffiliationsInternal(consortiumId, tenantId, centralTenantId);

    verify(createPrimaryAffiliationService, timeout(2000)).createPrimaryUserAffiliations(consortiumId, centralTenantId, tenantId, syncUsers);
    verify(tenantService).updateTenantSetupStatus(tenantId, centralTenantId, SetupStatusEnum.COMPLETED);
  }

  @Test
  void syncPrimaryAffiliationsGetUsersException() {
    var consortiumId = UUID.randomUUID();
    var tenantId = "ABC1";
    var centralTenantId = "diku";

    when(userService.getPrimaryUsersToLink())
      .thenThrow(HttpClientErrorException.class);

    syncPrimaryAffiliationService.syncPrimaryAffiliationsInternal(consortiumId, tenantId, centralTenantId);

    verify(tenantService).updateTenantSetupStatus(tenantId, centralTenantId, SetupStatusEnum.FAILED);
  }

  @Test
  void createPrimaryAffiliationsException() {
    var consortiumId = UUID.randomUUID();
    var tenantId = "ABC1";
    var centralTenantId = "diku";
    var syncUser = new SyncUser().id(UUID.randomUUID()
        .toString())
      .username("test_user");

    when(tenantService.getByTenantId(anyString())).thenThrow(DataAccessResourceFailureException.class);

    Executable call = () -> createPrimaryAffiliationService.createPrimaryUserAffiliations(consortiumId, centralTenantId, tenantId, List.of(syncUser));

    assertThrows(DataAccessResourceFailureException.class, call);
    verifyNoInteractions(primaryAffiliationService);
    verify(tenantService).updateTenantSetupStatus(tenantId, centralTenantId, SetupStatusEnum.FAILED);
    verify(lockService).lockTenantSetupWithinTransaction();
  }

  @Test
  void createPrimaryAffiliationsPartialFailure() {
    var consortiumId = UUID.randomUUID();
    var tenantId = "ABC1";
    var centralTenantId = "diku";
    TenantEntity tenantEntity1 = createTenantEntity(tenantId, "TestName1");
    tenantEntity1.setConsortiumId(consortiumId);


    var syncUser = new SyncUser().id("88888888-8888-4888-8888-888888888888").username("mockuser8");
    var syncUser2 = new SyncUser().id("99999999-9999-4999-9999-999999999999").username("mockuser9");

    when(tenantService.getByTenantId(anyString())).thenReturn(tenantEntity1);
    when(userTenantRepository.findByUserIdAndIsPrimaryTrue(any()))
      .thenReturn(Optional.empty())
      .thenThrow(DataAccessResourceFailureException.class);

    createPrimaryAffiliationService.createPrimaryUserAffiliations(consortiumId, centralTenantId, tenantId, List.of(syncUser, syncUser2));

    verify(primaryAffiliationService, times(1)).createPrimaryAffiliationInNewTransaction(any(), anyString(), any(), any());
    verify(tenantService).updateTenantSetupStatus(tenantId, centralTenantId, SetupStatusEnum.COMPLETED_WITH_ERRORS);
    verify(lockService).lockTenantSetupWithinTransaction();
  }

  @Test
  void syncPrimaryAffiliationsWithNoUsersToSync() {
    var consortiumId = UUID.randomUUID();
    var tenantId = "ABC1";
    var centralTenantId = "diku";

    when(userService.getPrimaryUsersToLink()).thenReturn(List.of());
    doNothing().when(tenantService).updateTenantSetupStatus(tenantId, centralTenantId, SetupStatusEnum.COMPLETED);

    syncPrimaryAffiliationService.syncPrimaryAffiliationsInternal(consortiumId, tenantId, centralTenantId);

    verify(tenantService).updateTenantSetupStatus(tenantId, centralTenantId, SetupStatusEnum.COMPLETED);
    verifyNoInteractions(createPrimaryAffiliationService);
  }

}
