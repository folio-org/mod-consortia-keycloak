package org.folio.consortia.controller;

import static org.folio.consortia.utils.EntityUtils.CENTRAL_TENANT_ID;
import static org.folio.consortia.utils.EntityUtils.createUserTenant;
import static org.folio.consortia.utils.EntityUtils.createUserTenantEntity;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.folio.consortia.client.UsersKeycloakClient;
import org.folio.consortia.domain.entity.UserTenantEntity;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.ConsortiumRepository;
import org.folio.consortia.repository.UserTenantRepository;
import org.folio.consortia.service.TenantService;
import org.folio.consortia.service.UserTenantService;
import java.nio.charset.Charset;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import org.folio.consortia.domain.dto.UserTenant;
import org.folio.consortia.domain.dto.UserTenantCollection;
import org.folio.consortia.support.BaseIT;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import feign.FeignException;
import feign.Request;
import feign.RequestTemplate;
import feign.Response;

@EntityScan(basePackageClasses = UserTenantEntity.class)
class UserTenantControllerTest extends BaseIT {

  private static final String CONSORTIUM_ID = "7698e46-c3e3-11ed-afa1-0242ac120002";
  private static final String PERMISSION_EXCEPTION_MSG = "[403 Forbidden] during [GET] to " +
    "[http://users/8c54ff1e-5954-4227-8402-9a5dd061a350] [UsersClient#getUsersByUserId(String)]: " +
    "[Access for user 'ss_admin' (b82b46b6-9a6e-46f0-b986-5c643d9ba036) requires permission: users.item.get]";
  @Mock
  private UserTenantService userTenantService;
  @InjectMocks
  private UserTenantController userTenantController;
  @MockBean
  private ConsortiumRepository consortiumRepository;
  @MockBean
  private UserTenantRepository userTenantRepository;
  @MockBean
  private TenantService tenantService;
  @SpyBean
  private UsersKeycloakClient usersKeycloakClient;

  @Test
  void shouldGetUserTenantsByUserId() {
    // given
    UUID consortiumId = UUID.randomUUID();
    UUID userId = UUID.randomUUID();
    List<UserTenant> userTenantDtos = List.of(new UserTenant(), new UserTenant());
    UserTenantCollection userTenantCollection = new UserTenantCollection();
    userTenantCollection.setUserTenants(userTenantDtos);
    userTenantCollection.setTotalRecords(userTenantDtos.size());

    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    when(userTenantService.getByUserId(UUID.fromString(CONSORTIUM_ID), userId, 0, 10)).thenReturn(userTenantCollection);

    // when
    ResponseEntity<UserTenantCollection> response = userTenantController.getUserTenants(UUID.fromString(CONSORTIUM_ID), userId, null, null, 0, 10);

    // then
    Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    Assertions.assertEquals(userTenantCollection, response.getBody());

    verify(userTenantService).getByUserId(UUID.fromString(CONSORTIUM_ID), userId, 0, 10);
  }

  @Test
  void shouldGetUserTenantByAssociationId() {
    // given
    UUID associationId = UUID.randomUUID();
    UUID consortiumId = UUID.randomUUID();
    UserTenant userTenant = createUserTenant(associationId);

    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    when(userTenantService.getById(consortiumId, associationId)).thenReturn(userTenant);

    // when
    ResponseEntity<UserTenant> response = userTenantController.getUserTenantByAssociationId(consortiumId, associationId);

    // then
    Assertions.assertEquals(HttpStatus.OK, response.getStatusCode());
    Assertions.assertEquals(userTenant, response.getBody());

    verify(userTenantService).getById(consortiumId, associationId);
  }

  @Test
  void shouldGetUserTenantList() throws Exception {
    var headers = defaultHeaders();
    UUID consortiumId = UUID.fromString(CONSORTIUM_ID);
    Page<UserTenantEntity> userTenantPage = new PageImpl<>(List.of(createUserTenantEntity(consortiumId)));

    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    when(userTenantRepository.findAll(PageRequest.of(1, 2))).thenReturn(userTenantPage);

    this.mockMvc.perform(
        get("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/user-tenants?limit=2&offset=1")
          .headers(headers))
      .andExpectAll(status().isOk(), content().contentType(MediaType.APPLICATION_JSON_VALUE));
  }

  /* Error cases */
  @Test
  void returnNotFoundUserAndTenantAssociationWhenDeletingUserTenant() throws Exception {
    var headers = defaultHeaders();
    when(consortiumRepository.existsById(UUID.fromString(CONSORTIUM_ID))).thenReturn(true);
    this.mockMvc.perform(delete("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/user-tenants?userId=7698e46-c3e3-11ed-afa1-0242ac120001&tenantId=diku")
        .headers(headers))
      .andExpect(status().isNotFound())
      .andExpect(content().contentType(MediaType.APPLICATION_JSON_VALUE))
      .andExpect(jsonPath("$.errors[0].code", is("NOT_FOUND_ERROR")));
  }

  @Test
  void return404NotFound() throws Exception {
    var headers = defaultHeaders();
    UUID consortiumId = UUID.fromString("7698e46-c3e3-11ed-afa1-0242ac120001");
    when(consortiumRepository.existsById(consortiumId)).thenReturn(false);

    this.mockMvc.perform(get("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/user-tenants?limit=0&offset=0")
        .headers(headers))
      .andExpectAll(status().is4xxClientError(),
        content().contentType(MediaType.APPLICATION_JSON_VALUE),
        jsonPath("$.errors[0].code", is("NOT_FOUND_ERROR")));
  }

  @Test
  void returnPermissionRequiredException() throws Exception {
    var headers = defaultHeaders();
    UUID consortiumId = UUID.fromString(CONSORTIUM_ID);
    UserTenantEntity userTenantEntity = createUserTenantEntity(consortiumId);

    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    when(userTenantRepository.findByUserIdAndTenantId(any(), any())).thenReturn(Optional.of(userTenantEntity));
    doNothing().when(userTenantRepository).deleteByUserIdAndTenantId(any(), any());
    doThrow(FeignException.Forbidden.errorStatus("getByUserId", createForbiddenResponse(PERMISSION_EXCEPTION_MSG)))
      .when(usersKeycloakClient).getUsersByUserId(any());

    this.mockMvc.perform(
        delete("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/user-tenants?userId=7698e46-c3e3-11ed-afa1-0242ac120001&tenantId=diku")
          .headers(headers))
      .andExpectAll(status().is4xxClientError(),
        content().contentType(MediaType.APPLICATION_JSON_VALUE),
        jsonPath("$.errors[0].code", is("PERMISSION_REQUIRED")));
  }

  @Test
  void returnIllegalStateExceptionWhileGettingUser() throws Exception {
    var headers = defaultHeaders();
    UUID consortiumId = UUID.fromString(CONSORTIUM_ID);
    UserTenantEntity userTenantEntity = createUserTenantEntity(consortiumId);

    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    when(userTenantRepository.findByUserIdAndTenantId(any(), any())).thenReturn(Optional.of(userTenantEntity));
    when(tenantService.getCentralTenantId()).thenReturn(CENTRAL_TENANT_ID);
    doNothing().when(userTenantRepository).deleteByUserIdAndTenantId(any(), any());
    doThrow(FeignException.errorStatus("getByUserId", createUnknownResponse("network error")))
      .when(usersKeycloakClient).getUsersByUserId(any());

    this.mockMvc.perform(
        delete("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/user-tenants?userId=7698e46-c3e3-11ed-afa1-0242ac120001&tenantId=diku")
          .headers(headers))
      .andExpectAll(status().is5xxServerError(),
        content().contentType(MediaType.APPLICATION_JSON_VALUE),
        jsonPath("$.errors[0].code", is("BAD_GATEWAY")));
  }

  @ParameterizedTest
  @CsvSource({
    "/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/user-tenants/cb28f43c-bf45-11ed-afa1-0242ac120002, NOT_FOUND_ERROR",
    "/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/user-tenants?userId=8ad4c4b4-4d4c-4bf9-a8a0-7a30c1edf34b, NOT_FOUND_ERROR",
    "/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/user-tenants?userId=90unnn, VALIDATION_ERROR"
  })
  void getUserTenant_shouldReturnExpectedStatusAndError(String path, String errorCode) throws Exception {
    var headers = defaultHeaders();
    UUID consortiumId = UUID.fromString(CONSORTIUM_ID);
    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    when(userTenantRepository.findByUserId(any(), any())).thenThrow(ResourceNotFoundException.class);

    this.mockMvc.perform(get(path).headers(headers))
      .andExpectAll(status().is4xxClientError(),
        content().contentType(MediaType.APPLICATION_JSON_VALUE),
        jsonPath("$.errors[0].code", is(errorCode)));
  }

  private Response createForbiddenResponse(String message) {
    Request request = Request.create(Request.HttpMethod.GET, "", Map.of(), null, Charset.defaultCharset(),
      new RequestTemplate());
    return Response.builder()
      .status(HttpStatus.FORBIDDEN.value())
      .body(message, Charset.defaultCharset())
      .request(request)
      .build();
  }
  private Response createUnknownResponse(String message) {
    Request request = Request.create(Request.HttpMethod.GET, "", Map.of(), null, Charset.defaultCharset(),
      new RequestTemplate());
    return Response.builder()
      .status(HttpStatus.BAD_GATEWAY.value())
      .body(message, Charset.defaultCharset())
      .request(request)
      .build();
  }
}
