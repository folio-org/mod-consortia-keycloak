package org.folio.consortia.service;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.folio.consortia.support.EntityUtils.createUserEntity;
import static org.folio.consortia.support.TestConstants.USER_ID;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import org.folio.consortia.domain.converter.UserTenantConverter;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.dto.UserEvent;
import org.folio.consortia.domain.dto.UserTenant;
import org.folio.consortia.domain.dto.UserTenantCollection;
import org.folio.consortia.domain.entity.TenantEntity;
import org.folio.consortia.domain.entity.UserTenantEntity;
import org.folio.consortia.exception.ConsortiumClientException;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.exception.UserAffiliationException;
import org.folio.consortia.repository.UserTenantRepository;
import org.folio.consortia.service.impl.UserTenantServiceImpl;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.data.OffsetRequest;
import org.folio.spring.integration.XOkapiHeaders;
import org.folio.spring.service.SystemUserScopedExecutionService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.invocation.InvocationOnMock;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.batch.BatchAutoConfiguration;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.convert.ConversionService;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;

@SpringBootTest
@EnableAutoConfiguration(exclude = BatchAutoConfiguration.class)
@EntityScan(basePackageClasses = UserTenantEntity.class)
class UserTenantServiceTest {

  private static final String CONSORTIUM_ID = "7698e46-c3e3-11ed-afa1-0242ac120002";

  @InjectMocks
  private UserTenantServiceImpl userTenantService;
  @Mock
  private UserTenantRepository userTenantRepository;
  @Mock
  private FolioExecutionContext folioExecutionContext;
  @Mock
  private ConversionService conversionService;
  @Mock
  private ConsortiumService consortiumService;
  @Mock
  private UserService userService;
  @Mock
  private FolioModuleMetadata folioModuleMetadata;
  @Mock
  private CapabilitiesUserService capabilitiesUserService;
  @Mock
  private TenantService tenantService;
  @Mock
  private SystemUserScopedExecutionService systemUserScopedExecutionService;

  /* Success cases */
  @Test
  void shouldGetUserTenantList() {
    // given
    List<UserTenantEntity> userTenantEntities = List.of(new UserTenantEntity(), new UserTenantEntity());
    Page<UserTenantEntity> userTenantPage = new PageImpl<>(userTenantEntities, OffsetRequest.of(0, 10), userTenantEntities.size());

    when(userTenantRepository.getAll(OffsetRequest.of(0, 10))).thenReturn(userTenantPage);

    // when
    var result = userTenantService.get(UUID.fromString(CONSORTIUM_ID), 0, 10);

    // then
    assertEquals(userTenantEntities.size(), result.getUserTenants().size());
    assertEquals(2, result.getTotalRecords());
  }

  @Test
  void shouldGetUserTenantByAssociationId() {
    // given
    UUID associationId = UUID.randomUUID();
    UUID userId = UUID.randomUUID();
    String tenantId = String.valueOf(UUID.randomUUID());
    UserTenantEntity userTenant = createUserTenantEntity(associationId, userId, "testuser", tenantId);
    List<UserTenantEntity> userTenantEntities = List.of(userTenant);

    when(conversionService.convert(userTenant, UserTenant.class)).thenReturn(toDto(userTenant));
    when(userTenantRepository.findById(associationId)).thenReturn(Optional.of(userTenantEntities.get(0)));

    // when
    var result = userTenantService.getById(UUID.fromString(CONSORTIUM_ID), associationId);

    // then
    assertEquals(associationId, result.getId());
    assertEquals("testuser", result.getUsername());
  }

  @Test
  void shouldGetUserTenantListByUserId() {
    // given
    UUID userId = UUID.randomUUID();
    UUID associationId = UUID.randomUUID();
    String tenantId = String.valueOf(UUID.randomUUID());

    UserTenantEntity userTenant = createUserTenantEntity(associationId, userId, "testuser", tenantId);
    UserTenantEntity userTenant2 = createUserTenantEntity(associationId, userId, "testuser", tenantId);
    List<UserTenantEntity> userTenantEntities = List.of(userTenant);

    when(conversionService.convert(userTenant, UserTenant.class)).thenReturn(toDto(userTenant));
    when(conversionService.convert(userTenant2, UserTenant.class)).thenReturn(toDto(userTenant2));
    when(userTenantRepository.findByUserId(userId, OffsetRequest.of(0, 10)))
      .thenReturn(new PageImpl<>(userTenantEntities, OffsetRequest.of(0, 10), userTenantEntities.size()));

    // when
    UserTenantCollection result = userTenantService.getByUserId(UUID.fromString(CONSORTIUM_ID), userId, 0, 10);

    // then
    assertEquals(userTenant2, userTenant);
    assertEquals(userTenantEntities.size(), result.getUserTenants().size());
    assertEquals(1, result.getTotalRecords());
  }

  @Test
  void shouldGetUserTenantByUsernameAndTenantId() {
    // given
    UUID userId = UUID.randomUUID();
    UUID associationId = UUID.randomUUID();
    String tenantId = String.valueOf(UUID.randomUUID());
    UserTenantEntity userTenant = createUserTenantEntity(associationId, userId, "testuser", tenantId);

    when(conversionService.convert(userTenant, UserTenant.class)).thenReturn(toDto(userTenant));
    when(userTenantRepository.findByUsernameAndTenantId("testuser", tenantId)).thenReturn(Optional.of(userTenant));

    // when
    UserTenantCollection result = userTenantService.getByUsernameAndTenantId(UUID.fromString(CONSORTIUM_ID), "testuser", tenantId);

    // then
    assertEquals(tenantId, result.getUserTenants().get(0).getTenantId());
    assertEquals(1, result.getTotalRecords());
  }

  @Test
  void shouldSavePrimaryAffiliation() {
    var consId = UUID.randomUUID();
    var userEvent = createUserEvent();
    ArgumentCaptor<UserTenantEntity> argCaptor = ArgumentCaptor.forClass(UserTenantEntity.class);
    when(userTenantRepository.save(argCaptor.capture())).thenAnswer(i -> i.getArguments()[0]);

    var result = userTenantService.createPrimaryUserTenantAffiliation(consId, new TenantEntity(), userEvent.getUserDto().getId(), userEvent.getUserDto().getUsername());
    assertNull(result);
  }

  @Test
  void shouldUpdateUsernameInPrimaryAffiliation() {
    var userEvent = createUserEvent();
    doNothing().when(userTenantRepository).setUsernameByUserIdAndTenantId(anyString(), any(), anyString());

    userTenantService.updateUsernameInPrimaryUserTenantAffiliation(UUID.fromString(userEvent.getUserDto().getId()), "newUsername", userEvent.getTenantId());

    verify(userTenantRepository, times(1)).setUsernameByUserIdAndTenantId(anyString(), any(), anyString());
  }

  @Test
  void shouldUpdateFirstAndLastNames() {
    UUID userId = USER_ID;
    String tenantId = "diku";
    UUID associationId = UUID.randomUUID();
    User primaryUser = createUserEntity(userId);
    User shadowUser = createUserEntity(userId);
    shadowUser.getPersonal().setFirstName("notUpdatedFirstName");
    shadowUser.getPersonal().setFirstName("notUpdatedLastName");
    User updatedShadowUser = createUserEntity(userId);
    UserTenantEntity userTenant = createUserTenantEntity(associationId, userId, "user", "shadowTenantId");
    userTenant.setIsPrimary(false);

    // validation part
    mockOkapiHeaders();

    // Returned object when expected parameter passed
    when(userTenantRepository.getByUserIdAndIsPrimaryFalse(userId)).thenReturn(List.of(userTenant));
    // In first call it return primary User, in second call it return shadow user.
    when(userService.getById(userId)).thenReturn(primaryUser).thenReturn(shadowUser);
    doNothing().when(userService).updateUser(updatedShadowUser);
    userTenantService.updateShadowUsersNameAndEmail(userId, tenantId);

    verify(userService, times(2)).getById(userId);
    verify(userService, times(1)).updateUser(updatedShadowUser);
  }

  @Test
  void shouldNotDoAnyActionWhenEmptyUserTenantEntityListReturned() {
    UUID userId = USER_ID;
    String tenantId = "diku";

    List<UserTenantEntity> emptyListOfUserTenantEntities = new ArrayList<>();

    // validation part
    mockOkapiHeaders();

    // Returned object when expected parameter passed
    when(userTenantRepository.getByUserIdAndIsPrimaryFalse(userId)).thenReturn(emptyListOfUserTenantEntities);
    userTenantService.updateShadowUsersNameAndEmail(userId, tenantId);

    verify(userService, times(0)).updateUser(any());
    verify(userService, times(0)).getById(any());
  }

  @Test
  void shouldThrowResourceNotFoundException() {
    UUID userId = UUID.randomUUID();
    String tenantId = UUID.randomUUID().toString();

    when(userTenantRepository.findByUserIdAndTenantId(any(), anyString())).thenReturn(Optional.empty());

    assertThrows(ResourceNotFoundException.class, () -> userTenantService.getByUserIdAndTenantId(userId, tenantId));

    verify(userTenantRepository, times(1)).findByUserIdAndTenantId(any(), anyString());
  }

  @Test
  void shouldDeletePrimaryAffiliation() {
    var userEvent = createUserEvent();
    when(userTenantRepository.deleteByUserIdAndIsPrimaryTrue(any())).thenReturn(1);

    boolean deleted = userTenantService.deletePrimaryUserTenantAffiliation(UUID.fromString(userEvent.getUserDto().getId()));

    verify(userTenantRepository, times(1)).deleteByUserIdAndIsPrimaryTrue(any());
    assertTrue(deleted);
  }

  @Test
  void shouldNotDeletePrimaryAffiliation() {
    var userEvent = createUserEvent();
    when(userTenantRepository.deleteByUserIdAndIsPrimaryTrue(any())).thenReturn(0);

    boolean deleted = userTenantService.deletePrimaryUserTenantAffiliation(UUID.fromString(userEvent.getUserDto().getId()));

    verify(userTenantRepository, times(1)).deleteByUserIdAndIsPrimaryTrue(any());
    assertFalse(deleted);
  }

  @Test
  void shouldSaveUserTenant() {
    UserTenant tenant = createUserTenantDtoEntity();
    UUID associationId = UUID.randomUUID();
    UUID userId = UUID.randomUUID();
    String tenantId = String.valueOf(UUID.randomUUID());
    UserTenantEntity userTenant = createUserTenantEntity(associationId, userId, "testuser", tenantId);
    userTenant.setIsPrimary(true);

    when(userTenantRepository.findByUserIdAndIsPrimaryTrue(any())).thenReturn(Optional.of(userTenant));
    when(userService.getById(any())).thenReturn(createUserEntity(false));
    when(userService.prepareShadowUser(any(), any())).thenReturn(createUserEntity(false));
    when(userTenantRepository.save(userTenant)).thenReturn(userTenant);
    mockOkapiHeaders();

    assertDoesNotThrow(() -> userTenantService.save(UUID.fromString(CONSORTIUM_ID), tenant, false));
  }
  @Test
  void shouldSaveUserTenantWithSystemUserContext() {
    UserTenant tenant = createUserTenantDtoEntity();
    UUID associationId = UUID.randomUUID();
    UUID userId = UUID.randomUUID();
    String tenantId = String.valueOf(UUID.randomUUID());
    UserTenantEntity userTenant = createUserTenantEntity(associationId, userId, "testuser", tenantId);
    userTenant.setIsPrimary(true);

    when(userTenantRepository.findByUserIdAndIsPrimaryTrue(any())).thenReturn(Optional.of(userTenant));
    when(userService.getById(any())).thenReturn(createUserEntity(false));
    when(userService.prepareShadowUser(any(), any())).thenReturn(createUserEntity(false));
    when(userTenantRepository.save(userTenant)).thenReturn(userTenant);
    doAnswer(UserTenantServiceTest::runSecondArgument)
      .when(systemUserScopedExecutionService).executeAsyncSystemUserScoped(eq("diku"), any());
    mockOkapiHeaders();

    assertDoesNotThrow(() -> userTenantService.save(UUID.fromString(CONSORTIUM_ID), tenant, true));
  }

  @Test
  void shouldUpdateUserAndSaveUserTenant() {
    UserTenant tenant = createUserTenantDtoEntity();
    UUID associationId = UUID.randomUUID();
    UUID userId = UUID.randomUUID();
    String tenantId = String.valueOf(UUID.randomUUID());
    UserTenantEntity userTenant = createUserTenantEntity(associationId, userId, "testuser", tenantId);
    userTenant.setIsPrimary(true);

    when(userTenantRepository.findByUserIdAndIsPrimaryTrue(any())).thenReturn(Optional.of(userTenant));
    when(userService.getById(any())).thenReturn(createUserEntity(true));
    when(userService.prepareShadowUser(any(), any())).thenReturn(createUserEntity(true));
    when(userTenantRepository.save(userTenant)).thenReturn(userTenant);
    mockOkapiHeaders();

    assertDoesNotThrow(() -> userTenantService.save(UUID.fromString(CONSORTIUM_ID), tenant, false));
  }

  @Test
  void shouldCreateUserWithPermissionSetAndSaveUserTenant() {
    UserTenant tenant = createUserTenantDtoEntity();
    UUID associationId = UUID.randomUUID();
    UUID userId = UUID.randomUUID();
    String tenantId = String.valueOf(UUID.randomUUID());
    UserTenantEntity userTenant = createUserTenantEntity(associationId, userId, "testuser", tenantId);
    userTenant.setIsPrimary(false);
    when(userTenantRepository.findByUserIdAndIsPrimaryTrue(any())).thenReturn(Optional.of(userTenant));
    when(userService.getById(any())).thenReturn(createNullUserEntity());
    when(userService.prepareShadowUser(any(), any())).thenReturn(createNullUserEntity());
    when(userTenantRepository.save(userTenant)).thenReturn(userTenant);
    mockOkapiHeaders();

    assertDoesNotThrow(() -> userTenantService.save(UUID.fromString(CONSORTIUM_ID), tenant, false));
  }

  @Test
  void shouldRemoveAllOrphanedShadowUsers() {
    UUID associationId = UUID.randomUUID();
    UUID userId1 = UUID.randomUUID();
    String tenantId1 = String.valueOf(UUID.randomUUID());
    String tenantId2 = String.valueOf(UUID.randomUUID());
    UserTenantEntity userTenant1 = createUserTenantEntity(associationId, userId1, "testuser1", tenantId1);
    UserTenantEntity userTenant2 = createUserTenantEntity(associationId, userId1, "testuser2", tenantId1);
    UserTenantEntity userTenant3 = createUserTenantEntity(associationId, userId1, "testuser3", tenantId2);
    userTenant1.setIsPrimary(false);
    userTenant2.setIsPrimary(false);
    userTenant3.setIsPrimary(false);
    when(userTenantRepository.getOrphansByUserIdAndIsPrimaryFalse(any())).thenReturn(List.of(userTenant1, userTenant2, userTenant3));
    mockOkapiHeaders();

    assertDoesNotThrow(() -> userTenantService.deleteShadowUsers(userId1));
    verify(capabilitiesUserService, times(3)).deleteUserCapabilitiesAndRoles(userId1.toString());
  }

  @Test
  void shouldDeleteUserTenantByUserIdAndTenantId() {
    UUID userId = UUID.randomUUID();
    String tenantId = "dikue";
    UUID associationId = UUID.randomUUID();
    UserTenantEntity userTenant = createUserTenantEntity(associationId, userId, "user", tenantId);
    userTenant.setIsPrimary(false);

    when(userService.getById(any())).thenReturn(createNullUserEntity());
    when(userService.prepareShadowUser(any(), any())).thenReturn(createNullUserEntity());
    when(userTenantRepository.findByUserIdAndTenantId(userId, tenantId)).thenReturn(Optional.of(userTenant));
    doNothing().when(userTenantRepository).deleteByUserIdAndTenantId(userId, tenantId);
    mockOkapiHeaders();

    assertDoesNotThrow(() -> userTenantService.deleteByUserIdAndTenantId(UUID.fromString(CONSORTIUM_ID), tenantId, userId));
  }

  /* Exception Cases */
  @Test
  void shouldThrowIllegalArgumentException() {
    UUID id = UUID.fromString(CONSORTIUM_ID);
    Assertions.assertThrows(IllegalArgumentException.class, () -> userTenantService.get(id, 0, 0));
  }

  @Test
  void shouldReturn404UserIdNotFoundException() {
    // given
    UUID userId = UUID.randomUUID();

    when(userTenantRepository.findByUserId(userId, OffsetRequest.of(0, 10))).thenReturn(new PageImpl<>(new ArrayList<>()));

    UUID id = UUID.fromString(CONSORTIUM_ID);
    // throw exception
    assertThrows(ResourceNotFoundException.class, () -> userTenantService.getByUserId(id, userId, 0, 10));
  }

  @Test
  void shouldReturn404UsernameNotFoundException() {
    // given
    String username = "testuser";
    String tenantId = String.valueOf(UUID.randomUUID());

    when(userTenantRepository.findByUsernameAndTenantId(username, tenantId)).thenReturn(Optional.empty());
    UUID id = UUID.fromString(CONSORTIUM_ID);

    // throw exception
    assertThrows(ResourceNotFoundException.class, () -> userTenantService.getByUsernameAndTenantId(id, "testusername", tenantId));
  }

  @Test
  void shouldThrowNotFoundPrimaryAffiliationException() {
    UserTenant tenant = createUserTenantDtoEntity();
    UUID associationId = UUID.randomUUID();

    when(userTenantRepository.findByUserIdAndIsPrimaryTrue(any())).thenReturn(Optional.empty());
    mockOkapiHeaders();

    assertThrows(ResourceNotFoundException.class, () -> userTenantService.save(associationId, tenant, false));
  }

  /* Error Cases */
  @Test
  void getByUsernameAndTenantIdNotFound() {
    doNothing().when(consortiumService).checkConsortiumExistsOrThrow(any());
    when(userTenantRepository.findByUserIdAndIsPrimaryTrue(any())).thenReturn(Optional.empty());

    var result = userTenantService.checkUserIfHasPrimaryAffiliationByUserId(UUID.randomUUID(), String.valueOf(UUID.randomUUID()));

    assertFalse(result);
  }

  @Test
  void shouldFailWhileDeletingPrimaryAffiliation() {
    UUID userId = UUID.randomUUID();
    String tenantId = "dikue";
    UUID associationId = UUID.randomUUID();
    UserTenantEntity userTenant = createUserTenantEntity(associationId, userId, "user", tenantId);
    userTenant.setIsPrimary(true);

    when(userTenantRepository.findByUserIdAndTenantId(userId, tenantId)).thenReturn(Optional.of(userTenant));
    doNothing().when(userTenantRepository).deleteByUserIdAndTenantId(userId, tenantId);
    mockOkapiHeaders();

    Assertions.assertThrows(UserAffiliationException.class, () -> userTenantService.deleteByUserIdAndTenantId(UUID.fromString(CONSORTIUM_ID), tenantId, userId),
      String.format(UserAffiliationException.USER_HAS_PRIMARY_AFFILIATION_WITH_TENANT, userId, tenantId));
  }

  @Test
  void shouldFailWhenDeletingCentralAffiliation() {
    UUID userId = UUID.randomUUID();
    String tenantId = "university";
    when(userTenantRepository.findByUserIdAndTenantId(userId, tenantId))
      .thenReturn(Optional.of(createUserTenantEntity(UUID.randomUUID(), userId, "user", tenantId)));
    when(tenantService.getCentralTenantId()).thenReturn(tenantId);

    Assertions.assertThrows(UserAffiliationException.class, () -> userTenantService.deleteByUserIdAndTenantId(UUID.fromString(CONSORTIUM_ID), tenantId, userId),
      String.format(UserAffiliationException.AFFILIATION_FROM_CENTRAL_TENANT_CAN_NOT_BE_DELETED, userId, tenantId));
  }

  @Test
  void shouldThrowConsortiumClientException() {
    UserTenant tenant = createUserTenantDtoEntity();
    UUID associationId = UUID.randomUUID();
    UUID userId = UUID.randomUUID();
    String tenantId = String.valueOf(UUID.randomUUID());
    UserTenantEntity userTenant = createUserTenantEntity(associationId, userId, "testuser", tenantId);
    userTenant.setIsPrimary(true);

    when(userTenantRepository.findByUserIdAndIsPrimaryTrue(any())).thenReturn(Optional.of(userTenant));
    when(userService.getById(any())).thenThrow(ConsortiumClientException.class);
    when(userTenantRepository.save(userTenant)).thenReturn(userTenant);
    mockOkapiHeaders();

    assertThrows(
      ConsortiumClientException.class, () -> userTenantService.save(UUID.fromString(CONSORTIUM_ID), tenant, false));
  }

  @ParameterizedTest
  @ValueSource(strings = { "testuser1", "testuser2", "testuser3" })
  void shouldThrowNotFoundIfUserNotFound(String username) {
    UserTenant tenant = createUserTenantDtoEntity();
    UUID associationId = UUID.randomUUID();
    UUID userId = UUID.randomUUID();
    String tenantId = String.valueOf(UUID.randomUUID());
    UserTenantEntity userTenant = createUserTenantEntity(associationId, userId, username, tenantId);
    userTenant.setIsPrimary(true);

    when(userTenantRepository.findByUserIdAndIsPrimaryTrue(any())).thenReturn(Optional.of(userTenant));
    when(userService.getById(any())).thenThrow(ResourceNotFoundException.class);
    when(userTenantRepository.save(userTenant)).thenReturn(userTenant);
    mockOkapiHeaders();

    assertThrows(
      ResourceNotFoundException.class, () -> userTenantService.save(UUID.fromString(CONSORTIUM_ID), tenant, false));
  }

  @ParameterizedTest
  @ValueSource(strings = { "testuser1" })
  void shouldThrowIllegalStateExceptionFromUserClient(String username) {
    UserTenant tenant = createUserTenantDtoEntity();
    UUID associationId = UUID.randomUUID();
    UUID userId = UUID.randomUUID();
    String tenantId = String.valueOf(UUID.randomUUID());
    UserTenantEntity userTenant = createUserTenantEntity(associationId, userId, username, tenantId);
    userTenant.setIsPrimary(true);

    when(userTenantRepository.findByUserIdAndIsPrimaryTrue(any())).thenReturn(Optional.of(userTenant));
    when(userService.getById(any())).thenThrow(java.lang.IllegalStateException.class);
    mockOkapiHeaders();

    assertThrows(java.lang.IllegalStateException.class,
      () -> userTenantService.save(UUID.fromString(CONSORTIUM_ID), tenant, false));
  }

  @Test
  void checkUserIfHasPrimaryAffiliationByUserIdSuccess() {
    var utEntity = createUserTenantEntity(UUID.randomUUID(), UUID.randomUUID(), "username", "diku");

    doNothing().when(consortiumService).checkConsortiumExistsOrThrow(any());
    when(conversionService.convert(any(), eq(UserTenant.class))).thenReturn(toDto(utEntity));
    when(userTenantRepository.findByUserIdAndIsPrimaryTrue(any())).thenReturn(Optional.of(utEntity));

    var result = userTenantService.checkUserIfHasPrimaryAffiliationByUserId(UUID.randomUUID(), String.valueOf(UUID.randomUUID()));

    assertTrue(result);
  }

  @Test
  void userHasPrimaryAffiliationByUsernameAndTenantId_positive() {
    when(userTenantRepository.existsByUsernameAndTenantIdAndIsPrimaryTrue(anyString(), anyString())).thenReturn(true);

    var result = userTenantService.userHasPrimaryAffiliationByUsernameAndTenantId("username", "tenantId");
    assertThat(result).isTrue();
  }

  private UserTenantEntity createUserTenantEntity(UUID associationId, UUID userId, String username, String tenantId) {
    UserTenantEntity userTenantEntity = new UserTenantEntity();
    userTenantEntity.setId(associationId);
    userTenantEntity.setUserId(userId);

    var tenant = new TenantEntity();
    tenant.setId(tenantId);
    tenant.setName("testtenant");
    tenant.setConsortiumId(UUID.fromString(CONSORTIUM_ID));
    userTenantEntity.setTenant(tenant);
    userTenantEntity.setUsername(username);
    return userTenantEntity;
  }

  private UserTenant toDto(UserTenantEntity userTenantEntity) {
    UserTenantConverter tenantConverter = new UserTenantConverter();
    return tenantConverter.convert(userTenantEntity);
  }

  private UserTenant createUserTenantDtoEntity() {
    UserTenant tenant = new UserTenant();
    tenant.setTenantId("diku");
    tenant.setTenantName("diku");
    tenant.setUserId(UUID.randomUUID());
    tenant.setId(UUID.randomUUID());
    tenant.setUsername("Test");
    tenant.setIsPrimary(false);

    return tenant;
  }

  private UserEvent createUserEvent() {
    var userEvent = new UserEvent();
    userEvent.userDto(new User().id(UUID.randomUUID().toString()).username("userName"));
    userEvent.setTenantId(UUID.randomUUID().toString());
    return userEvent;
  }

  private User createNullUserEntity() {
    User user = new User();
    user.setId(UUID.randomUUID().toString());
    return user;
  }

  private void mockOkapiHeaders() {
    when(folioExecutionContext.getTenantId()).thenReturn("diku");
    Map<String, Collection<String>> okapiHeaders = new HashMap<>();
    okapiHeaders.put(XOkapiHeaders.TENANT, List.of("diku"));
    when(folioExecutionContext.getOkapiHeaders()).thenReturn(okapiHeaders);
  }

  private static Object runSecondArgument(InvocationOnMock invocation) {
    invocation.<Runnable>getArgument(1).run();
    return null;
  }
}
