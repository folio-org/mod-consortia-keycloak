package org.folio.consortia.service;

import static org.folio.consortia.service.impl.CustomFieldServiceImpl.ORIGINAL_TENANT_ID_CUSTOM_FIELD;
import static org.folio.consortia.support.EntityUtils.CENTRAL_TENANT_ID;
import static org.folio.consortia.support.EntityUtils.TENANT_ID;
import static org.folio.consortia.support.EntityUtils.createOkapiHeaders;
import static org.folio.consortia.support.EntityUtils.createTenant;
import static org.folio.consortia.support.EntityUtils.createTenantDeleteRequest;
import static org.folio.consortia.support.EntityUtils.createTenantDetailsEntity;
import static org.folio.consortia.support.EntityUtils.createTenantEntity;
import static org.folio.consortia.support.EntityUtils.createUser;
import static org.folio.consortia.support.EntityUtils.getFolioExecutionContext;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.Callable;
import org.folio.consortia.client.ConsortiaConfigurationClient;
import org.folio.consortia.client.UserTenantsClient;
import org.folio.consortia.domain.dto.ConsortiaConfiguration;
import org.folio.consortia.domain.dto.IdentityProviderCreateRequest;
import org.folio.consortia.domain.dto.Tenant;
import org.folio.consortia.domain.dto.TenantDeleteRequest.DeleteTypeEnum;
import org.folio.consortia.domain.dto.TenantDetails;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.dto.UserTenantCollection;
import org.folio.consortia.domain.entity.TenantDetailsEntity;
import org.folio.consortia.domain.entity.TenantEntity;
import org.folio.consortia.domain.entity.UserTenantEntity;
import org.folio.consortia.exception.ResourceAlreadyExistException;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.TenantDetailsRepository;
import org.folio.consortia.repository.TenantRepository;
import org.folio.consortia.repository.UserTenantRepository;
import org.folio.consortia.service.impl.TenantManagerImpl;
import org.folio.consortia.service.impl.TenantServiceImpl;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.context.ExecutionContextBuilder;
import org.folio.spring.data.OffsetRequest;
import org.folio.spring.integration.XOkapiHeaders;
import org.folio.spring.service.SystemUserScopedExecutionService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.Mock;
import org.mockito.Spy;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.batch.BatchAutoConfiguration;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.convert.ConversionService;
import org.springframework.data.domain.PageImpl;

@SpringBootTest
@EnableAutoConfiguration(exclude = BatchAutoConfiguration.class)
@EntityScan(basePackageClasses = TenantEntity.class)
class TenantManagerTest {

  private static final UUID CONSORTIUM_ID = UUID.fromString("7698e46-c3e3-11ed-afa1-0242ac120002");

  @Mock
  private TenantRepository tenantRepository;
  @Mock
  private TenantDetailsRepository tenantDetailsRepository;
  @Mock
  private UserTenantRepository userTenantRepository;
  @Mock
  private ConversionService conversionService;
  @Mock
  private ConsortiumService consortiumService;
  @Spy
  private FolioExecutionContext folioExecutionContext = getFolioExecutionContext();
  @Mock
  private ConsortiaConfigurationClient consortiaConfigurationClient;
  @Mock
  private CapabilitiesUserService capabilitiesUserService;
  @Mock
  private UserService userService;
  @Mock
  private ExecutionContextBuilder executionContextBuilder;
  @Mock
  private UserTenantsClient userTenantsClient;
  @Mock
  private SyncPrimaryAffiliationService syncPrimaryAffiliationService;
  @Mock
  private CleanupService cleanupService;
  @Mock
  private LockService lockService;
  @Mock
  private SystemUserScopedExecutionService systemUserScopedExecutionService;
  @Mock
  private CustomFieldService customFieldService;
  @Mock
  private KeycloakService keycloakService;

  private TenantManager tenantManager;
  private TenantService tenantService;

  @BeforeEach
  void setUp() {
    tenantService = new TenantServiceImpl(tenantRepository, userTenantRepository, tenantDetailsRepository, conversionService, consortiumService, folioExecutionContext);
    tenantManager = new TenantManagerImpl(tenantService, keycloakService, consortiumService, consortiaConfigurationClient, syncPrimaryAffiliationService, userService, capabilitiesUserService,
      customFieldService, cleanupService, lockService, userTenantsClient, systemUserScopedExecutionService, executionContextBuilder, folioExecutionContext);
  }

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

    when(tenantRepository.existsById(any())).thenReturn(true);
    when(tenantRepository.findByConsortiumId(consortiumId)).thenReturn(tenantEntityList);

    var allTenants = tenantService.getAll(consortiumId);
    assertEquals(2, allTenants.getTotalRecords());
  }

  @Test
  void shouldSaveNotCentralTenantWithNewUserAndPermissions() {
    TenantDetailsEntity localTenantDetailsEntity = createTenantDetailsEntity("ABC1", "TestName1");
    Tenant tenant = createTenant("TestID", "Test");
    TenantEntity centralTenant = createTenantEntity("diku", "diku");
    User adminUser = createUser("diku_admin");

    when(userService.prepareShadowUser(UUID.fromString(adminUser.getId()), "diku")).thenReturn(adminUser);
    when(userService.createUser(any())).thenReturn(adminUser);
    when(userService.getById(any())).thenReturn(new User());
    when(tenantRepository.existsById(any())).thenReturn(false);
    when(tenantRepository.findCentralTenant()).thenReturn(Optional.of(centralTenant));
    when(tenantDetailsRepository.save(any(TenantDetailsEntity.class))).thenReturn(localTenantDetailsEntity);
    doNothing().when(consortiaConfigurationClient).saveConfiguration(any(ConsortiaConfiguration.class));
    doNothing().when(userTenantsClient).postUserTenant(any());
    when(userTenantsClient.getUserTenants()).thenReturn(new UserTenantCollection(List.of(), 0));
    when(conversionService.convert(localTenantDetailsEntity, Tenant.class)).thenReturn(tenant);
    when(folioExecutionContext.getTenantId()).thenReturn(TENANT_ID);
    when(customFieldService.getCustomFieldByName("originalTenantId")).thenReturn(ORIGINAL_TENANT_ID_CUSTOM_FIELD);
    when(systemUserScopedExecutionService.executeSystemUserScoped(eq("TestID"), any(Callable.class)))
      .thenAnswer(invocation -> {
        Callable<?> action = invocation.getArgument(1);
        return action.call();
      });

    var tenant1 = tenantManager.save(CONSORTIUM_ID, UUID.fromString(adminUser.getId()), tenant);

    verify(userService, times(1)).prepareShadowUser(UUID.fromString(adminUser.getId()), "diku");
    verify(userTenantRepository, times(1)).save(any());
    verify(consortiaConfigurationClient).saveConfiguration(any());
    verify(userTenantsClient).postUserTenant(any());
    verify(userService, times(1)).createUser(any());
    verify(lockService).lockTenantSetupWithinTransaction();
    verify(systemUserScopedExecutionService).executeSystemUserScoped(eq("TestID"), any());
    verify(customFieldService, never()).createCustomField(any());

    assertEquals(tenant, tenant1);
  }

  @Test
  void shouldSaveCentralTenantWithExistingAndPermissions() {
    TenantDetailsEntity tenantDetailsEntity = createTenantDetailsEntity("ABC1", "TestName1");
    Tenant tenant = createTenant("TestID", "Test", true);
    TenantEntity centralTenant = createTenantEntity(TENANT_ID);
    User user = new User();
    user.setId(UUID.randomUUID().toString());

    when(userService.prepareShadowUser(any(), any())).thenReturn(user);
    when(tenantRepository.existsById(any())).thenReturn(false);
    when(tenantRepository.findCentralTenant()).thenReturn(Optional.of(centralTenant));
    when(tenantDetailsRepository.save(any(TenantDetailsEntity.class))).thenReturn(tenantDetailsEntity);
    doNothing().when(consortiaConfigurationClient).saveConfiguration(any());
    doNothing().when(userTenantsClient).postUserTenant(any());
    when(conversionService.convert(tenantDetailsEntity, Tenant.class)).thenReturn(tenant);
    when(folioExecutionContext.getTenantId()).thenReturn("diku");
    Map<String, Collection<String>> okapiHeaders = new HashMap<>();
    okapiHeaders.put(XOkapiHeaders.TENANT, List.of("diku"));
    when(folioExecutionContext.getOkapiHeaders()).thenReturn(okapiHeaders);
    doReturn(folioExecutionContext).when(executionContextBuilder).buildContext(anyString());
    mockOkapiHeaders();
    when(customFieldService.getCustomFieldByName("originalTenantId")).thenReturn(null);
    when(systemUserScopedExecutionService.executeSystemUserScoped(eq("TestID"), any(Callable.class)))
      .thenAnswer(invocation -> {
        Callable<?> action = invocation.getArgument(1);
        return action.call();
      });

    var tenant1 = tenantManager.save(CONSORTIUM_ID, UUID.randomUUID(), tenant);

    verify(consortiaConfigurationClient).saveConfiguration(any());
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
    Tenant newTenant = createTenant("TestID", "Test", false);
    TenantEntity existedTenant = createTenantEntity("TestID");
    existedTenant.setIsDeleted(true);
    TenantDetailsEntity savedTenantDetailsEntity = createTenantDetailsEntity("ABC1", "TestName1");

    TenantEntity centralTenant = createTenantEntity(TENANT_ID);
    when(tenantRepository.findById(newTenant.getId())).thenReturn(Optional.of(existedTenant));
    when(tenantRepository.findCentralTenant()).thenReturn(Optional.of(centralTenant));
    when(tenantDetailsRepository.save(any(TenantDetailsEntity.class))).thenReturn(savedTenantDetailsEntity);
    doNothing().when(tenantDetailsRepository).setSetupStatusByTenantId(TenantDetails.SetupStatusEnum.COMPLETED, newTenant.getId());
    doNothing().when(userTenantsClient).postUserTenant(any());
    when(userTenantsClient.getUserTenants()).thenReturn(new UserTenantCollection(List.of(), 0));
    when(conversionService.convert(savedTenantDetailsEntity, Tenant.class)).thenReturn(newTenant);
    doReturn(folioExecutionContext).when(executionContextBuilder).buildContext(anyString());
    mockOkapiHeaders();

    var actualTenant = tenantManager.save(CONSORTIUM_ID, UUID.randomUUID(), newTenant);

    verifyNoInteractions(consortiaConfigurationClient);
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
    TenantEntity existingTenant = createTenantEntity("TestID", "TestName1");
    Tenant tenant = createTenant("TestID", "TestName2");

    when(tenantRepository.findById(any())).thenReturn(Optional.of(existingTenant));
    when(tenantRepository.save(any(TenantEntity.class))).thenReturn(existingTenant);
    when(conversionService.convert(existingTenant, Tenant.class)).thenReturn(tenant);
    mockOkapiHeaders();

    var tenant1 = tenantManager.update(CONSORTIUM_ID, tenant.getId(), tenant);
    Assertions.assertEquals(tenant.getId(), tenant1.getId());
    Assertions.assertEquals("TestName2", tenant1.getName());
  }

  @Test
  void testDeleteTenantNonExistent() {
    UUID consortiumId = UUID.randomUUID();
    String tenantId = "123";
    var deleteRequest = createTenantDeleteRequest(DeleteTypeEnum.SOFT, false);

    // Mock repository method calls
    when(tenantRepository.existsById(tenantId)).thenReturn(false);

    // Call the method
    assertThrows(ResourceNotFoundException.class, () ->
      tenantManager.delete(consortiumId, tenantId, deleteRequest));
  }

  @Test
  void testDeleteTenantSoft() {
    UUID consortiumId = UUID.randomUUID();
    var tenant = createTenantEntity(TENANT_ID);
    var deletingTenant = createTenantEntity(TENANT_ID);
    deletingTenant.setIsDeleted(true);
    var deleteRequest = createTenantDeleteRequest(DeleteTypeEnum.SOFT, true);

    doNothing().when(consortiumService).checkConsortiumExistsOrThrow(consortiumId);
    doNothing().when(cleanupService).clearPublicationTables();
    doReturn(deletingTenant).when(tenantRepository).save(deletingTenant);
    when(tenantRepository.findById(tenant.getId())).thenReturn(Optional.of(tenant));
    doReturn(folioExecutionContext).when(executionContextBuilder).buildContext(anyString());
    mockOkapiHeaders();

    tenantManager.delete(consortiumId, TENANT_ID, deleteRequest);

    // Assert
    verify(consortiumService).checkConsortiumExistsOrThrow(consortiumId);
    verify(tenantRepository).findById(TENANT_ID);
    verify(tenantRepository).save(deletingTenant);
    verify(cleanupService).clearPublicationTables();
    verify(userTenantsClient).deleteUserTenants();
  }

  @Test
  void testDeleteTenantSoftCentral() {
    UUID consortiumId = UUID.randomUUID();
    var tenant = createTenantEntity(TENANT_ID);
    tenant.setIsCentral(true);
    var deletingTenant = createTenantEntity(TENANT_ID);
    deletingTenant.setIsDeleted(true);
    var deleteRequest = createTenantDeleteRequest(DeleteTypeEnum.SOFT, false);

    doNothing().when(consortiumService).checkConsortiumExistsOrThrow(consortiumId);
    when(tenantRepository.findById(tenant.getId())).thenReturn(Optional.of(tenant));

    // Assert
    assertThrows(java.lang.IllegalArgumentException.class, () ->
      tenantManager.delete(consortiumId, TENANT_ID, deleteRequest));

    verify(consortiumService).checkConsortiumExistsOrThrow(consortiumId);
    verify(tenantRepository).findById(TENANT_ID);
    verifyNoInteractions(cleanupService);
    verifyNoInteractions(userTenantsClient);
  }

  @ParameterizedTest
  @CsvSource({"true, false", "false, false", "true, true", "false, true"})
  void testDeleteTenantHard(boolean deleteUserTenants, boolean isCentral) {
    UUID consortiumId = UUID.randomUUID();
    var tenant = createTenantEntity(TENANT_ID);
    tenant.setIsCentral(isCentral);
    var deleteRequest = createTenantDeleteRequest(DeleteTypeEnum.HARD, deleteUserTenants);

    if (deleteUserTenants) {
      doNothing().when(userTenantsClient).deleteUserTenants();
    }
    doNothing().when(consortiaConfigurationClient).deleteConfiguration();
    doNothing().when(cleanupService).clearSharingTables(TENANT_ID);
    doNothing().when(consortiumService).checkConsortiumExistsOrThrow(consortiumId);
    doNothing().when(cleanupService).clearPublicationTables();
    doNothing().when(tenantRepository).delete(tenant);
    doNothing().when(userTenantRepository).deleteUserTenantsByTenantId(TENANT_ID);
//    doNothing().when(keycloakService).deleteIdentityProvider(CENTRAL_TENANT_ID, TENANT_ID);
    mockOkapiHeaders();
    when(tenantRepository.findById(tenant.getId())).thenReturn(Optional.of(tenant));
    when(executionContextBuilder.buildContext(anyString())).thenReturn(folioExecutionContext);
    when(folioExecutionContext.getTenantId()).thenReturn(CENTRAL_TENANT_ID);

    tenantManager.delete(consortiumId, TENANT_ID, deleteRequest);

    var verifyTimes = deleteUserTenants ? times(1) : never();
    verify(userTenantsClient, verifyTimes).deleteUserTenants();

    verify(consortiaConfigurationClient).deleteConfiguration();
    verify(cleanupService).clearSharingTables(TENANT_ID);
    verify(consortiumService).checkConsortiumExistsOrThrow(consortiumId);
    verify(tenantRepository).findById(TENANT_ID);
    verify(cleanupService).clearPublicationTables();
    verify(tenantRepository).delete(tenant);
    verify(userTenantRepository).deleteUserTenantsByTenantId(TENANT_ID);
//    verify(keycloakService).deleteIdentityProvider(CENTRAL_TENANT_ID, TENANT_ID);
  }

  @Test
  void shouldThrowExceptionWhileSavingLocalTenantWithoutAdminUserId() {
    TenantDetailsEntity tenantDetailsEntity = createTenantDetailsEntity("TestID", "TestName1");
    Tenant tenant = createTenant("TestID", "TestName2");

    when(tenantDetailsRepository.save(any(TenantDetailsEntity.class))).thenReturn(tenantDetailsEntity);

    assertThrows(java.lang.IllegalArgumentException.class, () ->
      tenantManager.save(CONSORTIUM_ID, null, tenant));
  }

  @Test
  void shouldThrowExceptionWhileSavingWithDuplicateCodeOrName() {
    Tenant tenant = createTenant("TestID", "TestName2");

    when(tenantRepository.existsByCodeForOtherTenant(anyString(), anyString())).thenReturn(true);

    var adminUserId = UUID.randomUUID();
    assertThrows(ResourceAlreadyExistException.class, () ->
      tenantManager.save(CONSORTIUM_ID, adminUserId, tenant));
  }

  @Test
  void shouldThrowExceptionWhileUpdateTenant() {
    var existingTenant = createTenantEntity("TestID", "TestName1");
    var tenant = createTenant("TestID", "TestName2");

    when(tenantRepository.findById(tenant.getId() + "1234")).thenReturn(Optional.of(existingTenant));

    var tenantId = tenant.getId() + "1234";
    assertThrows(java.lang.IllegalArgumentException.class, () ->
      tenantManager.update(CONSORTIUM_ID, tenantId, tenant));
  }

  @Test
  void shouldThrowNotFoundExceptionWhileUpdateTenant() {
    TenantEntity tenantEntity1 = createTenantEntity("TestID", "TestName1");
    Tenant tenant = createTenant("TestID", "TestName2");

    when(conversionService.convert(tenantEntity1, Tenant.class)).thenReturn(tenant);

    var tenantId = tenant.getId() + "1234";
    assertThrows(ResourceNotFoundException.class, () ->
      tenantManager.update(CONSORTIUM_ID, tenantId, tenant));
  }

  @Test
  void shouldNotSaveExistingTenant() {
    Tenant tenant = createTenant("TestID", "Test");
    TenantEntity existedTenant = createTenantEntity("TestId");

    when(tenantRepository.findById(tenant.getId())).thenReturn(Optional.of(existedTenant));

    assertThrows(ResourceAlreadyExistException.class,
      () -> tenantManager.save(CONSORTIUM_ID, null, tenant));
  }

  @Test
  void shouldNotSaveDuplicateCentralTenant() {
    Tenant tenant = createTenant("TestID", "Testq", true);

    when(tenantRepository.existsByIsCentralTrue()).thenReturn(true);
    mockOkapiHeaders();

    assertThrows(ResourceAlreadyExistException.class, () ->
      tenantManager.save(CONSORTIUM_ID, null, tenant));
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

    when(tenantDetailsRepository.findById(TENANT_ID)).thenReturn(Optional.of(tenantDetailsEntity));
    when(conversionService.convert(tenantDetailsEntity, TenantDetails.class)).thenReturn(tenantDetailsExpected);

    var tenantDetails = tenantService.getTenantDetailsById(consortiumId, TENANT_ID);
    assertEquals(tenantDetailsExpected, tenantDetails);
  }

  @Test
  void testGetTenantDetailsNonExistingTenant() {
    UUID consortiumId = UUID.randomUUID();

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

    var adminUserId = UUID.fromString(adminUser.getId());
    assertThrows(RuntimeException.class, () -> tenantManager.save(consortiumId, adminUserId, tenant));
  }

  private void mockOkapiHeaders() {
    when(folioExecutionContext.getTenantId()).thenReturn("diku");
    Map<String, Collection<String>> okapiHeaders = createOkapiHeaders();
    when(folioExecutionContext.getOkapiHeaders()).thenReturn(okapiHeaders);
  }

  @Test
  void testAddNewTenantWithExistentUserTenant() {
    Tenant tenant = createTenant(TENANT_ID);
    TenantDetailsEntity tenantDetailsEntity = createTenantDetailsEntity(tenant.getId(), tenant.getName());
    TenantEntity centralTenant = createTenantEntity(CENTRAL_TENANT_ID);
    User adminUser = createUser("diku_admin");

    when(tenantRepository.findById(tenant.getId())).thenReturn(Optional.empty());
    when(tenantRepository.findCentralTenant()).thenReturn(Optional.of(centralTenant));
    when(tenantDetailsRepository.save(any(TenantDetailsEntity.class))).thenReturn(tenantDetailsEntity);
    when(conversionService.convert(tenantDetailsEntity, Tenant.class)).thenReturn(tenant);
    when(userService.prepareShadowUser(any(UUID.class), anyString())).thenReturn(adminUser);
    when(userTenantRepository.save(any(UserTenantEntity.class))).thenReturn(new UserTenantEntity());
    doNothing().when(keycloakService).createIdentityProvider(CENTRAL_TENANT_ID, TENANT_ID);
    doNothing().when(consortiaConfigurationClient).saveConfiguration(any());
    when(userTenantsClient.getUserTenants()).thenReturn(new UserTenantCollection(List.of(), 1));

    var tenant1 = tenantManager.save(CONSORTIUM_ID, UUID.fromString(adminUser.getId()), tenant);

    verify(userService, times(1)).prepareShadowUser(UUID.fromString(adminUser.getId()), "diku");
    verify(userTenantRepository, times(1)).save(any());
    verify(consortiaConfigurationClient).saveConfiguration(any());
    verify(lockService).lockTenantSetupWithinTransaction();
    verify(keycloakService).createIdentityProvider(CENTRAL_TENANT_ID, TENANT_ID);
    verify(userTenantsClient, never()).postUserTenant(any());
    verify(userService, never()).getById(any());
    verify(userService, never()).createUser(any());
    verify(capabilitiesUserService, never()).createWithPermissionSetsFromFile(any(), any());
    verify(customFieldService, never()).createCustomField(any());

    assertEquals(tenant, tenant1);
  }

  @Test
  void testReAddSoftDeletedTenantWithExistentUserTenant() {
    Tenant tenant = createTenant(TENANT_ID);
    TenantEntity tenantEntity = createTenantEntity(TENANT_ID);
    tenantEntity.setIsDeleted(true);
    TenantDetailsEntity tenantDetailsEntity = createTenantDetailsEntity(tenant.getId(), tenant.getName());
    TenantEntity centralTenant = createTenantEntity(CENTRAL_TENANT_ID);
    User adminUser = createUser("diku_admin");

    when(tenantRepository.findById(tenant.getId())).thenReturn(Optional.of(tenantEntity));
    when(tenantRepository.findCentralTenant()).thenReturn(Optional.of(centralTenant));
    when(tenantDetailsRepository.save(any(TenantDetailsEntity.class))).thenReturn(tenantDetailsEntity);
    when(conversionService.convert(tenantDetailsEntity, Tenant.class)).thenReturn(tenant);
    when(executionContextBuilder.buildContext(anyString())).thenReturn(folioExecutionContext);

    when(userTenantsClient.getUserTenants()).thenReturn(new UserTenantCollection(List.of(), 1));

    var tenant1 = tenantManager.save(CONSORTIUM_ID, UUID.fromString(adminUser.getId()), tenant);

    verify(userTenantsClient, never()).postUserTenant(any());
    verify(customFieldService, never()).createCustomField(any());
    verify(keycloakService, never()).createIdentityProvider(any(), any());

    assertEquals(tenant, tenant1);
  }

  @Test
  void testCreateIdentityProvider() {
    var idpCreateRequest = new IdentityProviderCreateRequest().createProvider(true).migrateUsers(true);
    when(folioExecutionContext.getTenantId()).thenReturn(CENTRAL_TENANT_ID);

    tenantManager.createIdentityProvider(TENANT_ID, idpCreateRequest);

    verify(keycloakService).createIdentityProvider(CENTRAL_TENANT_ID, TENANT_ID);
  }

  @Test
  void testDeleteIdentityProvider() {
    when(folioExecutionContext.getTenantId()).thenReturn(CENTRAL_TENANT_ID);

    tenantManager.deleteIdentityProvider(TENANT_ID);

    verify(keycloakService).deleteIdentityProvider(CENTRAL_TENANT_ID, TENANT_ID);
  }

  @Test
  void testSetupCustomLogin() {
    TenantEntity centralTenant = new TenantEntity();
    centralTenant.setIsCentral(true);
    centralTenant.setId(CENTRAL_TENANT_ID);

    when(tenantRepository.findById(CENTRAL_TENANT_ID)).thenReturn(Optional.of(centralTenant));

    tenantManager.setupCustomLogin(CONSORTIUM_ID, centralTenant.getId());

    verify(consortiumService).checkConsortiumExistsOrThrow(CONSORTIUM_ID);
    verify(tenantRepository).findById(centralTenant.getId());
    verify(keycloakService).addCustomAuthFlowForCentralTenant(centralTenant.getId());
  }

  @Test
  void testSetupCustomLoginTenantIsNotCentral() {
    TenantEntity centralTenant = new TenantEntity();
    centralTenant.setIsCentral(false);
    centralTenant.setId(CENTRAL_TENANT_ID);

    when(tenantRepository.findById(CENTRAL_TENANT_ID)).thenReturn(Optional.of(centralTenant));

    tenantManager.setupCustomLogin(CONSORTIUM_ID, centralTenant.getId());

    verify(consortiumService).checkConsortiumExistsOrThrow(CONSORTIUM_ID);
    verify(tenantRepository).findById(centralTenant.getId());
    verify(keycloakService, never()).addCustomAuthFlowForCentralTenant(anyString());
  }

  @Test
  void testSetupCustomLoginConsortiumNotExists() {
    doThrow(ResourceNotFoundException.class).when(consortiumService).checkConsortiumExistsOrThrow(CONSORTIUM_ID);

    assertThrows(ResourceNotFoundException.class, () -> tenantManager.setupCustomLogin(CONSORTIUM_ID, CENTRAL_TENANT_ID));

    verify(consortiumService).checkConsortiumExistsOrThrow(CONSORTIUM_ID);
    verify(tenantRepository, never()).findById(anyString());
    verify(keycloakService, never()).addCustomAuthFlowForCentralTenant(anyString());
  }

  @Test
  void testSetupCustomLoginTenantNotExists() {
    when(tenantRepository.findById(CENTRAL_TENANT_ID)).thenReturn(Optional.empty());

    assertThrows(ResourceNotFoundException.class, () -> tenantManager.setupCustomLogin(CONSORTIUM_ID, CENTRAL_TENANT_ID));

    verify(consortiumService).checkConsortiumExistsOrThrow(CONSORTIUM_ID);
    verify(tenantRepository).findById(anyString());
    verify(keycloakService, never()).addCustomAuthFlowForCentralTenant(anyString());
  }

}
