package org.folio.consortia.controller;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.urlPathMatching;
import static org.folio.consortia.support.EntityUtils.createConsortiaConfiguration;
import static org.folio.consortia.support.EntityUtils.createTenant;
import static org.folio.consortia.support.EntityUtils.createTenantDetailsEntity;
import static org.folio.consortia.support.EntityUtils.createTenantEntity;
import static org.folio.consortia.support.EntityUtils.createUser;
import static org.folio.consortia.support.EntityUtils.createUserTenantEntity;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.tomakehurst.wiremock.client.WireMock;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.ConstraintViolationException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import org.folio.consortia.client.CapabilitiesClient;
import org.folio.consortia.client.ConsortiaConfigurationClient;
import org.folio.consortia.client.SyncPrimaryAffiliationClient;
import org.folio.consortia.client.UserCapabilitiesClient;
import org.folio.consortia.client.UserPermissionsClient;
import org.folio.consortia.client.UserTenantsClient;
import org.folio.consortia.client.UsersKeycloakClient;
import org.folio.consortia.config.kafka.KafkaService;
import org.folio.consortia.domain.dto.Capabilities;
import org.folio.consortia.domain.dto.Capability;
import org.folio.consortia.domain.dto.PermissionUser;
import org.folio.consortia.domain.dto.PermissionUserCollection;
import org.folio.consortia.domain.dto.SyncPrimaryAffiliationBody;
import org.folio.consortia.domain.dto.SyncUser;
import org.folio.consortia.domain.dto.Tenant;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.entity.TenantDetailsEntity;
import org.folio.consortia.domain.entity.TenantEntity;
import org.folio.consortia.domain.entity.UserTenantEntity;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.ConsortiumRepository;
import org.folio.consortia.repository.TenantDetailsRepository;
import org.folio.consortia.repository.TenantRepository;
import org.folio.consortia.repository.UserTenantRepository;
import org.folio.consortia.service.TenantService;
import org.folio.consortia.service.UserService;
import org.folio.consortia.service.UserTenantService;
import org.folio.consortia.service.impl.ConsortiaConfigurationServiceImpl;
import org.folio.consortia.base.BaseIT;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.integration.XOkapiHeaders;
import org.folio.spring.service.PrepareSystemUserService;
import org.folio.spring.service.SystemUserScopedExecutionService;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.Mock;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

@EntityScan(basePackageClasses = TenantEntity.class)
class TenantControllerTest extends BaseIT {
  private static final String TENANT_REQUEST_BODY =
    "{\"id\":\"diku1234\",\"code\":\"TST\",\"name\":\"diku_tenant_name1234\", \"isCentral\":false}";
  private static final String CONSORTIUM_ID = "7698e46-c3e3-11ed-afa1-0242ac120002";
  private static final String CENTRAL_TENANT_ID = "diku";
  public static final String SYNC_PRIMARY_AFFILIATIONS_URL =
    "/consortia/%s/tenants/%s/sync-primary-affiliations?centralTenantId=%s";
  public static final String PRIMARY_AFFILIATIONS_URL =
    "/consortia/%s/tenants/%s/create-primary-affiliations?centralTenantId=%s";

  @MockBean
  ConsortiumRepository consortiumRepository;
  @MockBean
  TenantRepository tenantRepository;
  @MockBean
  TenantDetailsRepository tenantDetailsRepository;
  @MockBean
  UserTenantRepository userTenantRepository;
  @MockBean
  ConsortiaConfigurationServiceImpl configurationService;
  @MockBean
  ConsortiaConfigurationClient configurationClient;
  @MockBean
  KafkaService kafkaService;
  @MockBean
  UserTenantService userTenantService;
  @MockBean
  UserService userService;
  @MockBean
  PrepareSystemUserService prepareSystemUserService;
  @MockBean
  SystemUserScopedExecutionService systemUserScopedExecutionService;
  @Mock
  FolioModuleMetadata folioModuleMetadata;
  @Mock
  FolioExecutionContext folioExecutionContext = new FolioExecutionContext() {};
  @MockBean
  CapabilitiesClient capabilitiesClient;
  @MockBean
  UserCapabilitiesClient userCapabilitiesClient;
  @MockBean
  UserPermissionsClient userPermissionsClient;
  @MockBean
  UserTenantsClient userTenantsClient;
  @MockBean
  SyncPrimaryAffiliationClient syncPrimaryAffiliationClient;
  @MockBean
  UsersKeycloakClient usersKeycloakClient;

  /* Success cases */
  @Test
  void getTenants() throws Exception {
    UUID consortiumId = UUID.fromString(CONSORTIUM_ID);
    TenantEntity tenantEntity1 = createTenantEntity();
    TenantEntity tenantEntity2 = createTenantEntity();
    List<TenantEntity> tenantEntityList = new ArrayList<>();
    tenantEntityList.add(tenantEntity1);
    tenantEntityList.add(tenantEntity2);

    when(tenantRepository.findByConsortiumId(any(), any(PageRequest.of(0, 2)
      .getClass()))).thenReturn(new PageImpl<>(tenantEntityList, PageRequest.of(0, 2), tenantEntityList.size()));
    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    var headers = defaultHeaders();

    this.mockMvc.perform(
        get("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/tenants?limit=2&offset=1").headers(headers))
      .andExpectAll(status().isOk(), content().contentType(MediaType.APPLICATION_JSON_VALUE));
  }

  @ParameterizedTest
  @ValueSource(strings = {TENANT_REQUEST_BODY})
  void shouldSaveTenant(String contentString) throws Exception {
    var headers = defaultHeaders();
    String userId = UUID.randomUUID().toString();
    TenantEntity centralTenant = createTenantEntity(CENTRAL_TENANT_ID, CENTRAL_TENANT_ID, "AAA", true);
    PermissionUser permissionUser = new PermissionUser();
    permissionUser.setUserId(userId);
    permissionUser.setPermissions(List.of("test.permission"));
    PermissionUserCollection permissionUserCollection = new PermissionUserCollection();
    permissionUserCollection.setPermissionUsers(List.of(permissionUser));
    User adminUser = createUser("diku_admin");
    User systemUser = createUser("consortia-system-user");

    var tenantDetailsEntity = new TenantDetailsEntity();
    tenantDetailsEntity.setConsortiumId(centralTenant.getConsortiumId());
    tenantDetailsEntity.setId("diku1234");

    wireMockServer.stubFor(
      WireMock.get(urlPathMatching("https://permissions/users/.*"))
        .willReturn(aResponse()
          .withBody(asJsonString(new PermissionUser()))
          .withHeader(XOkapiHeaders.TOKEN, TOKEN)));

    doNothing().when(userTenantsClient).postUserTenant(any());
    when(userService.getByUsername("consortia-system-user")).thenReturn(Optional.of(systemUser));
    when(userService.prepareShadowUser(UUID.fromString(adminUser.getId()), TENANT)).thenReturn(adminUser);
    when(userService.prepareShadowUser(UUID.fromString(systemUser.getId()), TENANT)).thenReturn(systemUser);
    when(userService.getById(any())).thenReturn(adminUser);
    doReturn(new User()).when(usersKeycloakClient).getUsersByUserId(any());
    doReturn(getCapabilities()).when(capabilitiesClient).queryCapabilities(anyString(), anyInt(), anyInt());
    doNothing().when(userCapabilitiesClient).assignUserCapabilities(anyString(), any());
    when(consortiumRepository.existsById(any())).thenReturn(true);
    when(tenantRepository.existsById(any())).thenReturn(false);
    when(tenantDetailsRepository.save(any(TenantDetailsEntity.class))).thenReturn(tenantDetailsEntity);
    when(tenantRepository.findCentralTenant()).thenReturn(Optional.of(centralTenant));
    doNothing().when(syncPrimaryAffiliationClient).syncPrimaryAffiliations(anyString(), anyString(), anyString());
    doNothing().when(configurationClient).saveConfiguration(createConsortiaConfiguration(CENTRAL_TENANT_ID));

    this.mockMvc.perform(
        post("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/tenants?adminUserId=" + adminUser.getId())
          .headers(headers).content(contentString))
      .andExpect(status().isCreated());
  }

  @ParameterizedTest
  @ValueSource(strings = {TENANT_REQUEST_BODY})
  void shouldUpdateTenant(String contentString) throws Exception {
    TenantEntity tenant = createTenantEntity();

    var headers = defaultHeaders();
    when(tenantRepository.existsById(any())).thenReturn(true);
    when(consortiumRepository.existsById(any())).thenReturn(true);
    when(tenantRepository.save(any())).thenReturn(tenant);

    this.mockMvc.perform(
        put("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/tenants/diku1234")
          .headers(headers).content(contentString))
      .andExpectAll(status().isOk());
  }

  @Test
  void shouldGetTenantDetails() throws Exception {
    TenantDetailsEntity tenantDetailsEntity = createTenantDetailsEntity();

    var headers = defaultHeaders();
    when(tenantRepository.existsById(any())).thenReturn(true);
    when(consortiumRepository.existsById(any())).thenReturn(true);
    when(tenantDetailsRepository.findById(any())).thenReturn(Optional.of(tenantDetailsEntity));

    this.mockMvc.perform(
        get("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/tenants/diku1234")
          .headers(headers))
      .andExpectAll(status().isOk(), content().contentType(MediaType.APPLICATION_JSON_VALUE));
  }

  /* Error cases */
  @Test
  void getBadRequest() throws Exception {
    var headers = defaultHeaders();
    UUID consortiumId = UUID.fromString(CONSORTIUM_ID);

    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);

    this.mockMvc.perform(get("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/tenants?limit=0&offset=0")
        .headers(headers)).
      andExpectAll(
        status().is4xxClientError(),
        content().contentType(MediaType.APPLICATION_JSON_VALUE),
        jsonPath("$.errors[0].message", is("Page size must not be less than one")));
  }

  @Test
  void get4xxError() throws Exception {
    var headers = defaultHeaders();

    this.mockMvc.perform(get("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/tenants?limit=0&offset=0")
        .headers(headers))
      .andExpectAll(
        status().is4xxClientError(),
        content().contentType(MediaType.APPLICATION_JSON_VALUE),
        MockMvcResultMatchers.jsonPath("$.errors[0].message", Matchers.is(String.format(
          ResourceNotFoundException.NOT_FOUND_MSG_TEMPLATE, "consortiumId", "07698e46-c3e3-11ed-afa1-0242ac120002"))));
  }

  @ParameterizedTest
  @ValueSource(strings = {TENANT_REQUEST_BODY})
  void shouldGet4xxErrorWhileSaving(String contentString) throws Exception {
    var headers = defaultHeaders();
    TenantEntity centralTenant = createTenantEntity(CENTRAL_TENANT_ID, CENTRAL_TENANT_ID, "TTA", true);

    doReturn(new User()).when(usersKeycloakClient).getUsersByUserId(any());
    when(tenantRepository.findCentralTenant()).thenReturn(Optional.of(centralTenant));
    doNothing().when(configurationClient).saveConfiguration(createConsortiaConfiguration(CENTRAL_TENANT_ID));

    this.mockMvc.perform(post("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/tenants?adminUserId=111841e3-e6fb-4191-9fd8-5674a5107c34")
        .headers(headers).content(contentString))
      .andExpectAll(
        status().is4xxClientError(),
        MockMvcResultMatchers.jsonPath("$.errors[0].message", Matchers.is(String.format(
          ResourceNotFoundException.NOT_FOUND_MSG_TEMPLATE, "consortiumId","07698e46-c3e3-11ed-afa1-0242ac120002"))),
        jsonPath("$.errors[0].code", is("NOT_FOUND_ERROR")));
  }

  @ParameterizedTest
  @ValueSource(strings = {"{\"id\": \"123123123123123123\",\"code\":\"TST\", \"name\": \"\"}"})
    // isCentral is not given
  void shouldThrowMethodArgumentNotValidationException(String contentString) throws Exception {
    var headers = defaultHeaders();

    mockMvc.perform(post("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/tenants?adminUserId=111841e3-e6fb-4191-9fd8-5674a5107c34")
        .headers(headers)
        .contentType(MediaType.APPLICATION_JSON)
        .content(contentString))
      .andExpect(status().isUnprocessableEntity())
      .andExpect(jsonPath("$.errors.size()", is(2)))
      .andExpect(jsonPath("$.errors[0].code", is("tenantValidationError")));
  }

  @ParameterizedTest
  @ValueSource(strings = {"{\"id\": \"123123123123123123\",\"code\":\"@ST\", \"name\": \"\", \"isCentral\":false}"})
  void shouldThrownMethodArgumentNotValidException(String contentString) throws Exception {
    var headers = defaultHeaders();
    TenantEntity centralTenant = createTenantEntity(CENTRAL_TENANT_ID, CENTRAL_TENANT_ID, "TTA", true);

    // Given a request with invalid input
    UUID consortiumId = UUID.fromString(CONSORTIUM_ID);
    PermissionUser permissionUser = new PermissionUser();
    PermissionUserCollection permissionUserCollection = new PermissionUserCollection();
    permissionUserCollection.setPermissionUsers(List.of(permissionUser));

    doReturn(new User()).when(usersKeycloakClient).getUsersByUserId(any());
    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    when(tenantRepository.existsById(any(String.class))).thenReturn(false);
    when(tenantRepository.findCentralTenant()).thenReturn(Optional.of(centralTenant));
    doNothing().when(configurationClient).saveConfiguration(createConsortiaConfiguration(CENTRAL_TENANT_ID));

    Set<ConstraintViolation<?>> constraintViolations = new HashSet<>();
    constraintViolations.add(mock(ConstraintViolation.class));
    constraintViolations.add(mock(ConstraintViolation.class));

    when(tenantRepository.save(any(TenantEntity.class)))
      .thenThrow(new ConstraintViolationException("Invalid input", constraintViolations));

    // When performing a request to the endpoint
    mockMvc.perform(post("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/tenants?adminUserId=111841e3-e6fb-4191-9fd8-5674a5107c34")
        .headers(headers)
        .contentType(MediaType.APPLICATION_JSON)
        .content(contentString))
      .andExpect(status().isUnprocessableEntity())
      .andExpect(jsonPath("$.errors.size()", is(2)))
      .andExpect(jsonPath("$.errors[0].code", is("tenantValidationError")))
      .andExpect(jsonPath("$.errors[0].type", is("-1")))
      .andExpect(jsonPath("$.errors[1].code", is("tenantValidationError")))
      .andExpect(jsonPath("$.errors[1].type", is("-1")));
  }


  @ParameterizedTest
  @ValueSource(strings = {TENANT_REQUEST_BODY})
  void shouldGet4xxErrorWhileSavingDuplicateName(String contentString) throws Exception {
    var headers = defaultHeaders();
    UUID consortiumId = UUID.fromString(CONSORTIUM_ID);
    TenantEntity centralTenant = createTenantEntity(CENTRAL_TENANT_ID, CENTRAL_TENANT_ID, "TTA", true);
    PermissionUser permissionUser = new PermissionUser();
    PermissionUserCollection permissionUserCollection = new PermissionUserCollection();
    permissionUserCollection.setPermissionUsers(List.of(permissionUser));

    doReturn(new User()).when(usersKeycloakClient).getUsersByUserId(any());
    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    when(tenantRepository.existsById(any(String.class))).thenReturn(true);
    when(tenantRepository.findCentralTenant()).thenReturn(Optional.of(centralTenant));
    doNothing().when(configurationClient).saveConfiguration(createConsortiaConfiguration(CENTRAL_TENANT_ID));

    this.mockMvc.perform(
        post("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/tenants?adminUserId=111841e3-e6fb-4191-9fd8-5674a5107c34")
          .headers(headers).content(contentString))
      .andExpectAll(
        status().is4xxClientError(),
        jsonPath("$.errors[0].message", is("Object with id [diku1234] is already presented in the system")),
        jsonPath("$.errors[0].code", is("DUPLICATE_ERROR")));
  }

  @ParameterizedTest
  @ValueSource(strings = {TENANT_REQUEST_BODY})
  void shouldThrowValidationErrorWhileUpdateTenant(String contentString) throws Exception {
    var headers = defaultHeaders();

    when(tenantRepository.existsById(any())).thenReturn(true);
    when(consortiumRepository.existsById(any())).thenReturn(true);
    when(tenantRepository.save(any(TenantEntity.class))).thenReturn(TenantEntity.class.newInstance());

    this.mockMvc.perform(
        put("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/tenants/TestID")
          .headers(headers).content(contentString))
      .andExpectAll(
        status().is4xxClientError(),
        jsonPath("$.errors[0].message", is("Request body tenantId and path param tenantId should be identical")),
        jsonPath("$.errors[0].code", is("VALIDATION_ERROR")));
  }

  @ParameterizedTest
  @ValueSource(strings = {TENANT_REQUEST_BODY})
  void shouldThrowNotFoundErrorWhileUpdateTenant(String contentString) throws Exception {
    TenantEntity tenant = createTenantEntity();
    var headers = defaultHeaders();

    when(tenantRepository.existsById(any())).thenReturn(true);
    when(consortiumRepository.existsById(any())).thenReturn(false);
    when(tenantRepository.save(tenant)).thenReturn(tenant);

    this.mockMvc.perform(
        put("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/tenants/diku1234")
          .headers(headers).content(contentString))
      .andExpectAll(
        status().is4xxClientError(),
        jsonPath("$.errors[0].code", is("NOT_FOUND_ERROR")));
  }

  @ParameterizedTest
  @ValueSource(strings = {TENANT_REQUEST_BODY})
  void shouldThrownHasActiveAffiliationExceptionWhileDeletingTenant(String contentString) throws Exception {
    var headers = defaultHeaders();
    when(tenantRepository.existsById(any())).thenReturn(true);
    when(consortiumRepository.existsById(any())).thenReturn(true);
    when(userTenantRepository.existsByTenantId("diku1234")).thenReturn(true);

    this.mockMvc.perform(
        delete("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/tenants/diku1234")
          .headers(headers).content(contentString))
      .andExpectAll(
        status().is4xxClientError(),
        jsonPath("$.errors[0].code", is("VALIDATION_ERROR")));
  }

  @Test
  void syncPrimaryAffiliations() throws Exception {
    var headers = defaultHeaders();
    var consortiumId = UUID.randomUUID();
    var tenantId = "ABC1";
    var centralTenantId = "mobius";

    this.mockMvc
      .perform(post(String.format(SYNC_PRIMARY_AFFILIATIONS_URL, consortiumId, tenantId, centralTenantId)).headers(headers))
      .andExpectAll(status().isNoContent());
  }

  @Test
  void primaryAffiliation() throws Exception {
    TenantService tenantService = mock(TenantService.class);
    var headers = defaultHeaders();
    var consortiumId = UUID.randomUUID();
    var tenantId = "ABC1";
    var centralTenantId = "mobius";

    TenantEntity tenantEntity1 = createTenantEntity(tenantId, "TestName1");
    tenantEntity1.setConsortiumId(consortiumId);
    UserTenantEntity userTenantEntity = createUserTenantEntity(UUID.randomUUID());
    Tenant tenant = createTenant("TestID", "Test");

    var syncUser = new SyncUser().id(UUID.randomUUID()
        .toString())
      .username("test_user");
    var spab = new SyncPrimaryAffiliationBody()
      .users(Collections.singletonList(syncUser))
      .tenantId(tenantId);
    var spabString = new ObjectMapper().writeValueAsString(spab);

    when(tenantService.getCentralTenantId()).thenReturn(tenant.getId());
    when(tenantService.getByTenantId(anyString())).thenReturn(tenantEntity1);
    when(userTenantRepository.findByUserIdAndTenantId(any(), anyString())).thenReturn(Optional.of(userTenantEntity));

    this.mockMvc.perform(
        post(String.format(PRIMARY_AFFILIATIONS_URL, consortiumId, tenantId, centralTenantId)).headers(headers)
          .content(spabString))
      .andExpectAll(status().isNoContent());
  }

  private static Capabilities getCapabilities() {
    return new Capabilities().addCapabilitiesItem(new Capability().id(UUID.randomUUID()).permission("test.permission"));
  }
}
