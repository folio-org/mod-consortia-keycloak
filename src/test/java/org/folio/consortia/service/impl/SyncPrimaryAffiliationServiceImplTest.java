package org.folio.consortia.service.impl;

import static org.folio.consortia.support.EntityUtils.createTenantEntity;
import static org.folio.consortia.support.EntityUtils.getFolioExecutionContext;
import static org.folio.consortia.utils.InputOutputTestUtils.getMockDataAsString;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.timeout;
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
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.folio.consortia.domain.dto.SyncPrimaryAffiliationBody;
import org.folio.consortia.domain.dto.SyncUser;
import org.folio.consortia.domain.dto.TenantDetails.SetupStatusEnum;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.dto.UserCollection;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.data.OffsetRequest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.mockito.Mock;
import org.mockito.Spy;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.batch.BatchAutoConfiguration;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.task.AsyncTaskExecutor;
import org.springframework.core.task.SimpleAsyncTaskExecutor;
import org.springframework.dao.DataAccessResourceFailureException;
import org.springframework.data.domain.PageImpl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import feign.FeignException;

@SpringBootTest
@EnableAutoConfiguration(exclude = BatchAutoConfiguration.class)
@EntityScan(basePackageClasses = UserTenantEntity.class)
class SyncPrimaryAffiliationServiceImplTest {
  private static final String CQL_GET_USERS = "(cql.allRecords=1 NOT type=\"patron\" NOT type=\"dcb\" NOT type=\"shadow\" NOT type=\"system\")";

  @Mock
  private TenantRepository tenantRepository;
  @Mock
  private UserTenantRepository userTenantRepository;
  @Mock
  private TenantService tenantService;
  @Mock
  private UserService userService;
  @Mock
  private PrimaryAffiliationService primaryAffiliationService;
  @Mock
  private ConsortiaConfigurationService consortiaConfigurationService;
  @Mock
  private LockService lockService;
  @Spy
  private FolioExecutionContext folioExecutionContext = getFolioExecutionContext();
  @Spy
  private AsyncTaskExecutor asyncTaskExecutor = new SimpleAsyncTaskExecutor();

  private SyncPrimaryAffiliationServiceImpl syncPrimaryAffiliationService;

  @BeforeEach
  void setUp() {
    syncPrimaryAffiliationService = spy(new SyncPrimaryAffiliationServiceImpl(userService, tenantService, userTenantRepository,
      lockService, primaryAffiliationService, folioExecutionContext, asyncTaskExecutor));
    syncPrimaryAffiliationService.setSyncPrimaryAffiliationService(syncPrimaryAffiliationService);
  }

  @Test
  void createPrimaryUserAffiliationsWhenCentralTenantSaving() throws JsonProcessingException {
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
    var spab = new SyncPrimaryAffiliationBody()
      .users(Collections.singletonList(syncUser))
      .tenantId(tenantId);

    // stub collection of 2 users
    when(tenantService.getByTenantId(anyString())).thenReturn(tenantEntity1);
    when(userTenantRepository.findAnyByUserId(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
    when(tenantRepository.findById(anyString())).thenReturn(Optional.of(tenantEntity1));
    when(userService.getUsersByQuery(eq(CQL_GET_USERS), anyInt(), anyInt())).thenReturn(userCollection);
    when(consortiaConfigurationService.getCentralTenantId(anyString())).thenReturn(tenantId);

    syncPrimaryAffiliationService.createPrimaryUserAffiliationsInternal(consortiumId, centralTenantId, spab);

    verify(primaryAffiliationService).createPrimaryAffiliationInNewTransaction(any(), anyString(), any(), any());
    verify(tenantService).updateTenantSetupStatus(tenantId, centralTenantId, SetupStatusEnum.COMPLETED);
    verify(lockService).lockTenantSetupWithinTransaction();
  }
  @Test
  void createPrimaryUserAffiliationsWhenLocalTenantSaving() throws JsonProcessingException {
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
    var spab = new SyncPrimaryAffiliationBody()
      .users(Collections.singletonList(syncUser))
      .tenantId(tenantId);

    // stub collection of 2 users
    when(tenantService.getByTenantId(anyString())).thenReturn(tenantEntity1);
    when(userTenantRepository.findAnyByUserId(any(), any())).thenReturn(new PageImpl<>(Collections.emptyList()));
    when(tenantRepository.findById(anyString())).thenReturn(Optional.of(tenantEntity1));
    when(userService.getUsersByQuery(eq(CQL_GET_USERS), anyInt(), anyInt())).thenReturn(userCollection);
    when(consortiaConfigurationService.getCentralTenantId(anyString())).thenReturn(centralTenantId);

    syncPrimaryAffiliationService.createPrimaryUserAffiliationsInternal(consortiumId, centralTenantId, spab);

    verify(primaryAffiliationService).createPrimaryAffiliationInNewTransaction(any(), anyString(), any(), any());
    verify(tenantService).updateTenantSetupStatus(tenantId, centralTenantId, SetupStatusEnum.COMPLETED);
    verify(lockService).lockTenantSetupWithinTransaction();
  }

  @Test
  void syncPrimaryUserAffiliationsWhenTenantSaving() throws JsonProcessingException {
    var consortiumId = UUID.randomUUID();
    var tenantId = "ABC1";
    var centralTenantId = "diku";
    TenantEntity tenantEntity1 = createTenantEntity(tenantId, "TestName1");
    tenantEntity1.setConsortiumId(consortiumId);

    var userCollectionString = getMockDataAsString("mockdata/user_collection.json");
    List<User> userCollection = new ObjectMapper().readValue(userCollectionString, UserCollection.class).getUsers();

    var spab = getSyncBody(tenantId);

    doNothing().when(tenantService).updateTenantSetupStatus(tenantId, centralTenantId, SetupStatusEnum.COMPLETED);
    // stub collection of 2 users
    when(userService.getUsersByQuery(eq(CQL_GET_USERS), anyInt(), anyInt())).thenReturn(userCollection);
    // stub userTenantRepository to return page with 1 element for each user to skip affiliation creation
    userCollection.forEach(user ->
      when(userTenantRepository.findAnyByUserId(UUID.fromString(user.getId()), OffsetRequest.of(0, 1)))
        .thenReturn(new PageImpl<>(Collections.singletonList(new UserTenantEntity()))));

    syncPrimaryAffiliationService.syncPrimaryAffiliationsInternal(consortiumId, tenantId, centralTenantId);

    verify(syncPrimaryAffiliationService, timeout(2000)).createPrimaryUserAffiliationsInternal(consortiumId, centralTenantId, spab);
    verify(tenantService).updateTenantSetupStatus(tenantId, centralTenantId, SetupStatusEnum.COMPLETED);
  }

  @Test
  void syncPrimaryAffiliationsGetUsersException() {
    var consortiumId = UUID.randomUUID();
    var tenantId = "ABC1";
    var centralTenantId = "diku";

    when(userService.getUsersByQuery(eq(CQL_GET_USERS), anyInt(), anyInt()))
      .thenThrow(FeignException.FeignClientException.class);

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
    var spab = new SyncPrimaryAffiliationBody()
      .users(Collections.singletonList(syncUser))
      .tenantId(tenantId);

    when(tenantService.getByTenantId(anyString())).thenThrow(DataAccessResourceFailureException.class);

    Executable call = () -> syncPrimaryAffiliationService.createPrimaryUserAffiliationsInternal(consortiumId, centralTenantId, spab);

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
    var spab = new SyncPrimaryAffiliationBody()
      .users(List.of(syncUser, syncUser2))
      .tenantId(tenantId);

    when(tenantService.getByTenantId(anyString())).thenReturn(tenantEntity1)
      .thenThrow(DataAccessResourceFailureException.class);

    syncPrimaryAffiliationService.createPrimaryUserAffiliationsInternal(consortiumId, centralTenantId, spab);

    verifyNoInteractions(primaryAffiliationService);
    verify(tenantService).updateTenantSetupStatus(tenantId, centralTenantId, SetupStatusEnum.COMPLETED_WITH_ERRORS);
    verify(lockService).lockTenantSetupWithinTransaction();
  }

  private SyncPrimaryAffiliationBody getSyncBody(String tenantId) {
    var syncUser = new SyncUser()
      .id("88888888-8888-4888-8888-888888888888")
      .username("mockuser8")
      .email("hlintall1@si.edu")
      .phoneNumber("927-306-2327");
    var syncUser2 = new SyncUser()
      .id("99999999-9999-4999-9999-999999999999")
      .username("mockuser9")
      .externalSystemId("123")
      .barcode("test123")
      .email("mock@biglibrary.org")
      .phoneNumber("2125551212")
      .mobilePhoneNumber("112233");
    return new SyncPrimaryAffiliationBody()
      .users(List.of(syncUser, syncUser2))
      .tenantId(tenantId);
  }
}
