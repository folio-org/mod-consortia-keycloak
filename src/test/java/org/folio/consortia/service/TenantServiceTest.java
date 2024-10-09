package org.folio.consortia.service;

import static org.folio.consortia.service.impl.CustomFieldServiceImpl.ORIGINAL_TENANT_ID_CUSTOM_FIELD;
import static org.folio.consortia.support.EntityUtils.TENANT_ID;
import static org.folio.consortia.support.EntityUtils.createConsortiaConfiguration;
import static org.folio.consortia.support.EntityUtils.createOkapiHeaders;
import static org.folio.consortia.support.EntityUtils.createTenant;
import static org.folio.consortia.support.EntityUtils.createTenantDetailsEntity;
import static org.folio.consortia.support.EntityUtils.createTenantEntity;
import static org.folio.consortia.support.EntityUtils.createUser;
import static org.folio.consortia.utils.InputOutputTestUtils.getMockDataAsString;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.Callable;
import org.folio.consortia.client.ConsortiaConfigurationClient;
import org.folio.consortia.client.SyncPrimaryAffiliationClient;
import org.folio.consortia.client.UserTenantsClient;
import org.folio.consortia.client.UsersClient;
import org.folio.consortia.client.UsersKeycloakClient;
import org.folio.consortia.config.kafka.KafkaService;
import org.folio.consortia.domain.dto.Tenant;
import org.folio.consortia.domain.dto.TenantDetails;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.dto.UserCollection;
import org.folio.consortia.domain.entity.TenantDetailsEntity;
import org.folio.consortia.domain.entity.TenantEntity;
import org.folio.consortia.exception.ResourceAlreadyExistException;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.ConsortiumRepository;
import org.folio.consortia.repository.TenantDetailsRepository;
import org.folio.consortia.repository.TenantRepository;
import org.folio.consortia.repository.UserTenantRepository;
import org.folio.consortia.service.impl.TenantServiceImpl;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.context.ExecutionContextBuilder;
import org.folio.spring.data.OffsetRequest;
import org.folio.spring.integration.XOkapiHeaders;
import org.folio.spring.service.SystemUserScopedExecutionService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.batch.BatchAutoConfiguration;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.convert.ConversionService;
import org.springframework.data.domain.PageImpl;

@SpringBootTest
@EnableAutoConfiguration(exclude = BatchAutoConfiguration.class)
@EntityScan(basePackageClasses = TenantEntity.class)
class TenantServiceTest {

  private final static String CONSORTIUM_ID = "7698e46-c3e3-11ed-afa1-0242ac120002";

  @InjectMocks
  private TenantServiceImpl tenantService;
  @Mock
  private TenantRepository tenantRepository;
  @Mock
  private TenantDetailsRepository tenantDetailsRepository;
  @Mock
  private UserTenantRepository userTenantRepository;
  @Mock
  private UserAffiliationService userAffiliationService;
  @Mock
  private ConversionService conversionService;
  @Mock
  private ConsortiumRepository consortiumRepository;
  @Mock
  private ConsortiumService consortiumService;
  @Mock
  private FolioExecutionContext folioExecutionContext = new FolioExecutionContext() {
  };
  @Mock
  private ConsortiaConfigurationClient configurationClient;
  @Mock
  private KafkaService kafkaService;
  @Mock
  UsersKeycloakClient usersKeycloakClient;
  @Mock
  private UsersClient usersClient;
  @Mock
  UserTenantService userTenantService;
  @Mock
  private UserTenantsClient userTenantsClient;
  @Mock
  private CapabilitiesUserService capabilitiesUserService;
  @Mock
  private UserService userService;
  @Mock
  private SyncPrimaryAffiliationClient syncPrimaryAffiliationClient;
  @Mock
  FolioModuleMetadata folioModuleMetadata;
  @Mock
  CleanupService cleanupService;
  @Mock
  private SystemUserScopedExecutionService systemUserScopedExecutionService;
  @Mock
  private LockService lockService;
  @Mock
  private ExecutionContextBuilder executionContextBuilder;
  @Mock
  private CustomFieldService customFieldService;

  @Test
  void shouldGetTenantList() {
    int offset = 0;
    int limit = 2;
    UUID consortiumId = UUID.randomUUID();
    TenantEntity tenantEntity1 = createTenantEntity("ABC1", "TestName1");
    TenantEntity tenantEntity2 = createTenantEntity("ABC1", "TestName2");
    List<TenantEntity> tenantEntityList = new ArrayList<>();
    tenantEntityList.add(tenantEntity1);
    tenantEntityList.add(tenantEntity2);

    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    when(tenantRepository.existsById(any())).thenReturn(true);
    when(tenantRepository.findByConsortiumId(any(), any(OffsetRequest.of(offset, limit).getClass())))
      .thenReturn(new PageImpl<>(tenantEntityList, OffsetRequest.of(offset, limit), tenantEntityList.size()));

    var tenantCollection = tenantService.get(consortiumId, 0, 10);
    assertEquals(2, tenantCollection.getTotalRecords());
  }

  @Test
  void shouldGetAllTenantList() {
    UUID consortiumId = UUID.randomUUID();
    TenantEntity tenantEntity1 = createTenantEntity("tenant1", "tenant1");
    TenantEntity tenantEntity2 = createTenantEntity("tenant2", "tenant2");
    List<TenantEntity> tenantEntityList = new ArrayList<>();
    tenantEntityList.add(tenantEntity1);
    tenantEntityList.add(tenantEntity2);

    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    when(tenantRepository.existsById(any())).thenReturn(true);
    when(tenantRepository.findByConsortiumId(consortiumId)).thenReturn(tenantEntityList);

    var allTenants = tenantService.getAll(consortiumId);
    assertEquals(2, allTenants.getTotalRecords());
  }

  @Test
  void shouldSaveNotCentralTenantWithNewUserAndPermissions() {
    UUID consortiumId = UUID.fromString(CONSORTIUM_ID);
    TenantDetailsEntity localTenantDetailsEntity = createTenantDetailsEntity("ABC1", "TestName1");
    Tenant tenant = createTenant("TestID", "Test");
    TenantEntity centralTenant = createTenantEntity("diku", "diku");
    User adminUser = createUser("diku_admin");

    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    when(userService.prepareShadowUser(UUID.fromString(adminUser.getId()), "diku")).thenReturn(adminUser);
    when(userService.createUser(any())).thenReturn(adminUser);
    when(userService.getById(any())).thenReturn(new User());
    when(tenantRepository.existsById(any())).thenReturn(false);
    when(tenantRepository.findCentralTenant()).thenReturn(Optional.of(centralTenant));
    when(tenantDetailsRepository.save(any(TenantDetailsEntity.class))).thenReturn(localTenantDetailsEntity);
    doNothing().when(configurationClient).saveConfiguration(createConsortiaConfiguration(TENANT_ID));
    doNothing().when(userTenantsClient).postUserTenant(any());
    when(conversionService.convert(localTenantDetailsEntity, Tenant.class)).thenReturn(tenant);
    when(folioExecutionContext.getTenantId()).thenReturn(TENANT_ID);
    when(customFieldService.getCustomFieldByName("originalTenantId")).thenReturn(ORIGINAL_TENANT_ID_CUSTOM_FIELD);
    when(systemUserScopedExecutionService.executeSystemUserScoped(eq("TestID"), any(Callable.class)))
      .thenAnswer(invocation -> {
        Callable<?> action = invocation.getArgument(1);
        return action.call();
      });

    var tenant1 = tenantService.save(consortiumId, UUID.fromString(adminUser.getId()), tenant);

    verify(userService, times(1)).prepareShadowUser(UUID.fromString(adminUser.getId()), "diku");
    verify(userTenantRepository, times(1)).save(any());
    verify(configurationClient).saveConfiguration(any());
    verify(userTenantsClient).postUserTenant(any());
    verify(userService, times(1)).createUser(any());
    verify(lockService).lockTenantSetupWithinTransaction();
    verify(systemUserScopedExecutionService).executeSystemUserScoped(eq("TestID"), any());
    verify(customFieldService, never()).createCustomField(any());

    assertEquals(tenant, tenant1);
  }

  @Test
  void shouldSaveCentralTenantWithExistingAndPermissions() throws JsonProcessingException {
    UUID consortiumId = UUID.fromString(CONSORTIUM_ID);
    TenantDetailsEntity tenantDetailsEntity = createTenantDetailsEntity("ABC1", "TestName1");
    Tenant tenant = createTenant("TestID", "Test", true);
    TenantEntity centralTenant = createTenantEntity(TENANT_ID);
    User user = new User();
    user.setId(UUID.randomUUID().toString());
    var userCollectionString = getMockDataAsString("mockdata/user_collection.json");
    UserCollection userCollection = new ObjectMapper().readValue(userCollectionString, UserCollection.class);

    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    when(userService.prepareShadowUser(any(), any())).thenReturn(user);
    when(tenantRepository.existsById(any())).thenReturn(false);
    when(tenantRepository.findCentralTenant()).thenReturn(Optional.of(centralTenant));
    when(tenantDetailsRepository.save(any(TenantDetailsEntity.class))).thenReturn(tenantDetailsEntity);
    doNothing().when(configurationClient).saveConfiguration(createConsortiaConfiguration(TENANT_ID));
    doNothing().when(userTenantsClient).postUserTenant(any());
    when(conversionService.convert(tenantDetailsEntity, Tenant.class)).thenReturn(tenant);
    when(folioExecutionContext.getTenantId()).thenReturn("diku");
    Map<String, Collection<String>> okapiHeaders = new HashMap<>();
    okapiHeaders.put(XOkapiHeaders.TENANT, List.of("diku"));
    when(folioExecutionContext.getOkapiHeaders()).thenReturn(okapiHeaders);
    when(usersClient.getUserCollection(anyString(), anyInt(), anyInt())).thenReturn(userCollection);
    doReturn(folioExecutionContext).when(executionContextBuilder).buildContext(anyString());
    mockOkapiHeaders();
    when(customFieldService.getCustomFieldByName("originalTenantId")).thenReturn(null);
    when(systemUserScopedExecutionService.executeSystemUserScoped(eq("TestID"), any(Callable.class)))
      .thenAnswer(invocation -> {
        Callable<?> action = invocation.getArgument(1);
        return action.call();
      });

    var tenant1 = tenantService.save(consortiumId, UUID.randomUUID(), tenant);

    verify(configurationClient).saveConfiguration(any());
    verify(lockService).lockTenantSetupWithinTransaction();

    verify(userService, never()).prepareShadowUser(any(), any());
    verify(userTenantRepository, never()).save(any());
    verify(userTenantsClient, never()).postUserTenant(any());
    verify(userService, never()).createUser(any());
    verify(systemUserScopedExecutionService).executeSystemUserScoped(eq("TestID"), any());
    verify(customFieldService).createCustomField(ORIGINAL_TENANT_ID_CUSTOM_FIELD);
    verify(capabilitiesUserService, never()).createWithPermissionSetsFromFile(any(), any());

    assertEquals(tenant, tenant1);
  }

  @Test
  void shouldReAddSoftDeletedTenant() {
    UUID consortiumId = UUID.fromString(CONSORTIUM_ID);
    Tenant newTenant = createTenant("TestID", "Test", false);
    TenantEntity existedTenant = createTenantEntity("TestID");
    existedTenant.setIsDeleted(true);
    TenantDetailsEntity savedTenantDetailsEntity = createTenantDetailsEntity("ABC1", "TestName1");

    TenantEntity centralTenant = createTenantEntity(TENANT_ID);
    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    when(tenantRepository.findById(newTenant.getId())).thenReturn(Optional.of(existedTenant));
    when(tenantRepository.findCentralTenant()).thenReturn(Optional.of(centralTenant));
    when(tenantDetailsRepository.save(any(TenantDetailsEntity.class))).thenReturn(savedTenantDetailsEntity);
    doNothing().when(tenantDetailsRepository).setSetupStatusByTenantId(TenantDetails.SetupStatusEnum.COMPLETED, newTenant.getId());
    doNothing().when(userTenantsClient).postUserTenant(any());
    doNothing().when(userTenantsClient).postUserTenant(any());
    when(conversionService.convert(savedTenantDetailsEntity, Tenant.class)).thenReturn(newTenant);
    doReturn(folioExecutionContext).when(executionContextBuilder).buildContext(anyString());
    mockOkapiHeaders();

    var actualTenant = tenantService.save(consortiumId, UUID.randomUUID(), newTenant);

    verifyNoInteractions(configurationClient);
    verifyNoInteractions(lockService);

    verifyNoInteractions(userService);
    verifyNoInteractions(userTenantRepository);
    verify(userTenantsClient).postUserTenant(any());
    verifyNoInteractions(userService);
    verifyNoInteractions(capabilitiesUserService);

    assertEquals(newTenant, actualTenant);
  }

  @Test
  void shouldUpdateTenant() {
    UUID consortiumId = UUID.randomUUID();
    TenantEntity existingTenant = createTenantEntity("TestID", "TestName1");
    Tenant tenant = createTenant("TestID", "TestName2");

    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    when(tenantRepository.findById(any())).thenReturn(Optional.of(existingTenant));
    when(tenantRepository.save(any(TenantEntity.class))).thenReturn(existingTenant);
    when(conversionService.convert(existingTenant, Tenant.class)).thenReturn(tenant);
    mockOkapiHeaders();

    var tenant1 = tenantService.update(UUID.fromString(CONSORTIUM_ID), tenant.getId(), tenant);
    Assertions.assertEquals(tenant.getId(), tenant1.getId());
    Assertions.assertEquals("TestName2", tenant1.getName());
  }

  @Test
  void shouldDeleteTenant() {
    UUID consortiumId = UUID.randomUUID();
    var tenant = createTenantEntity(TENANT_ID);
    var deletingTenant = createTenantEntity(TENANT_ID);
    deletingTenant.setIsDeleted(true);

    doNothing().when(consortiumService).checkConsortiumExistsOrThrow(consortiumId);
    doNothing().when(cleanupService).clearPublicationTables();
    doReturn(deletingTenant).when(tenantRepository).save(deletingTenant);
    when(tenantRepository.findById(tenant.getId())).thenReturn(Optional.of(tenant));
    doReturn(folioExecutionContext).when(executionContextBuilder).buildContext(anyString());
    mockOkapiHeaders();

    tenantService.delete(consortiumId, TENANT_ID);

    // Assert
    verify(consortiumService).checkConsortiumExistsOrThrow(consortiumId);
    verify(tenantRepository).findById(TENANT_ID);
    verify(tenantRepository).save(deletingTenant);
    verify(cleanupService).clearPublicationTables();
    verify(userTenantsClient).deleteUserTenants();
  }

  @Test
  void testDeleteNonexistentTenant() {
    UUID consortiumId = UUID.randomUUID();
    String tenantId = "123";

    // Mock repository method calls
    when(tenantRepository.existsById(tenantId)).thenReturn(false);

    // Call the method
    assertThrows(ResourceNotFoundException.class, () ->
      tenantService.delete(consortiumId, tenantId));
  }

  @Test
  void shouldThrowErrorWhenDeletingCentralTenant() {
    UUID consortiumId = UUID.randomUUID();
    var tenant = createTenantEntity(TENANT_ID);
    tenant.setIsCentral(true);
    var deletingTenant = createTenantEntity(TENANT_ID);
    deletingTenant.setIsDeleted(true);

    doNothing().when(consortiumService).checkConsortiumExistsOrThrow(consortiumId);
    when(tenantRepository.findById(tenant.getId())).thenReturn(Optional.of(tenant));

    // Assert
    assertThrows(java.lang.IllegalArgumentException.class, () ->
      tenantService.delete(consortiumId, TENANT_ID));

    verify(consortiumService).checkConsortiumExistsOrThrow(consortiumId);
    verify(tenantRepository).findById(TENANT_ID);
    verifyNoInteractions(cleanupService);
    verifyNoInteractions(userTenantsClient);
  }


  @Test
  void shouldThrowExceptionWhileSavingLocalTenantWithoutAdminUserId() {
    TenantDetailsEntity tenantDetailsEntity = createTenantDetailsEntity("TestID", "TestName1");
    Tenant tenant = createTenant("TestID", "TestName2");

    when(consortiumRepository.existsById(any())).thenReturn(true);
    when(tenantRepository.existsById(any())).thenReturn(false);
    when(tenantDetailsRepository.save(any(TenantDetailsEntity.class))).thenReturn(tenantDetailsEntity);
    when(conversionService.convert(tenantDetailsEntity, Tenant.class)).thenReturn(tenant);

    assertThrows(java.lang.IllegalArgumentException.class, () ->
      tenantService.save(UUID.fromString(CONSORTIUM_ID), null, tenant));
  }

  @Test
  void shouldThrowExceptionWhileSavingWithDuplicateCodeOrName() {
    TenantEntity tenantEntity1 = createTenantEntity("TestID", "TestName1");
    Tenant tenant = createTenant("TestID", "TestName2");

    when(consortiumRepository.existsById(any())).thenReturn(true);
    when(tenantRepository.existsById(any())).thenReturn(false);
    when(tenantRepository.save(any(TenantEntity.class))).thenReturn(tenantEntity1);
    when(tenantRepository.existsByCodeForOtherTenant(anyString(), anyString())).thenReturn(true);
    when(conversionService.convert(tenantEntity1, Tenant.class)).thenReturn(tenant);

    assertThrows(ResourceAlreadyExistException.class, () ->
      tenantService.save(UUID.fromString(CONSORTIUM_ID), UUID.randomUUID(), tenant));
  }

  @Test
  void shouldThrowExceptionWhileUpdateTenant() {
    var existingTenant = createTenantEntity("TestID", "TestName1");
    var tenant = createTenant("TestID", "TestName2");

    when(consortiumRepository.existsById(any())).thenReturn(true);
    when(tenantRepository.findById(tenant.getId() + "1234")).thenReturn(Optional.of(existingTenant));

    assertThrows(java.lang.IllegalArgumentException.class, () ->
      tenantService.update(UUID.fromString(CONSORTIUM_ID), tenant.getId() + "1234", tenant));
  }

  @Test
  void shouldThrowNotFoundExceptionWhileUpdateTenant() {
    TenantEntity tenantEntity1 = createTenantEntity("TestID", "TestName1");
    Tenant tenant = createTenant("TestID", "TestName2");

    when(consortiumRepository.existsById(any())).thenReturn(true);
    when(conversionService.convert(tenantEntity1, Tenant.class)).thenReturn(tenant);

    assertThrows(ResourceNotFoundException.class, () ->
      tenantService.update(UUID.fromString(CONSORTIUM_ID), tenant.getId() + "1234", tenant));
  }

  @Test
  void shouldNotSaveExistingTenant() {
    UUID consortiumId = UUID.fromString(CONSORTIUM_ID);
    Tenant tenant = createTenant("TestID", "Test");
    TenantEntity existedTenant = createTenantEntity("TestId");

    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    when(tenantRepository.findById(tenant.getId())).thenReturn(Optional.of(existedTenant));

    assertThrows(ResourceAlreadyExistException.class,
      () -> tenantService.save(UUID.fromString(CONSORTIUM_ID), null, tenant));
  }

  @Test
  void shouldNotSaveDuplicateCentralTenant() {
    Tenant tenant = createTenant("TestID", "Testq", true);

    when(consortiumRepository.existsById(UUID.fromString(CONSORTIUM_ID))).thenReturn(true);
    when(tenantRepository.existsByIsCentralTrue()).thenReturn(true);
    mockOkapiHeaders();

    assertThrows(ResourceAlreadyExistException.class, () ->
      tenantService.save(UUID.fromString(CONSORTIUM_ID), null, tenant));
  }

  @Test
  void shouldRetrieveEntityByTenantId() {
    when(tenantRepository.findById(anyString())).thenReturn(Optional.of(new TenantEntity()));
    var tenantEntity = tenantService.getByTenantId(UUID.randomUUID().toString());
    assertNotNull(tenantEntity);
  }

  @Test
  void shouldNotRetrieveEntityByTenantId() {
    when(tenantRepository.findById(anyString())).thenReturn(Optional.empty());
    var tenantEntity = tenantService.getByTenantId(UUID.randomUUID().toString());
    assertNull(tenantEntity);
  }

  @Test
  void shouldGetTenantDetails() {
    UUID consortiumId = UUID.randomUUID();

    var tenantDetailsEntity = new TenantDetailsEntity();
    var tenantDetailsExpected = new TenantDetails();

    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    when(tenantDetailsRepository.findById(TENANT_ID)).thenReturn(Optional.of(tenantDetailsEntity));
    when(conversionService.convert(tenantDetailsEntity, TenantDetails.class)).thenReturn(tenantDetailsExpected);

    var tenantDetails = tenantService.getTenantDetailsById(consortiumId, TENANT_ID);
    assertEquals(tenantDetailsExpected, tenantDetails);
  }

  @Test
  void testGetTenantDetailsNonExistingTenant() {
    UUID consortiumId = UUID.randomUUID();

    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    when(tenantDetailsRepository.findById(TENANT_ID)).thenReturn(Optional.empty());

    assertThrows(ResourceNotFoundException.class, () ->
      tenantService.getTenantDetailsById(consortiumId, TENANT_ID));
  }

  @Test
  void shouldThrowExceptionWhileAddingTenant_customFieldCreationError() {
    UUID consortiumId = UUID.randomUUID();
    Tenant tenant = createTenant("TestID", "Test");
    User adminUser = createUser("diku_admin");

    when(customFieldService.getCustomFieldByName("originalTenantId")).thenReturn(null);
    when(systemUserScopedExecutionService.executeSystemUserScoped(eq("TestID"), any(Callable.class)))
      .thenAnswer(invocation -> {
        Callable<?> action = invocation.getArgument(1);
        return action.call();
      });
    doThrow(new RuntimeException("Error")).when(customFieldService).createCustomField(ORIGINAL_TENANT_ID_CUSTOM_FIELD);

    assertThrows(RuntimeException.class, () -> tenantService.save(consortiumId, UUID.fromString(adminUser.getId()), tenant));
  }

  private void mockOkapiHeaders() {
    when(folioExecutionContext.getTenantId()).thenReturn("diku");
    Map<String, Collection<String>> okapiHeaders = createOkapiHeaders();
    when(folioExecutionContext.getOkapiHeaders()).thenReturn(okapiHeaders);
  }
}
