package org.folio.consortia.service;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.folio.consortia.support.EntityUtils.createJsonNodeForGroupPayload;
import static org.folio.consortia.support.EntityUtils.createJsonNodeForRolePayload;
import static org.folio.consortia.support.EntityUtils.createPublicationDetails;
import static org.folio.consortia.support.EntityUtils.createPublicationRequest;
import static org.folio.consortia.support.EntityUtils.createPublicationResultCollection;
import static org.folio.consortia.support.EntityUtils.createSharingRoleResponse;
import static org.folio.consortia.support.EntityUtils.createSharingRoleResponseForDelete;
import static org.folio.consortia.support.EntityUtils.createTenant;
import static org.folio.consortia.support.EntityUtils.createTenantCollection;
import static org.folio.consortia.support.TestConstants.CONSORTIUM_ID;
import static org.folio.consortia.utils.InputOutputTestUtils.getMockDataObject;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Callable;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

import org.folio.consortia.domain.dto.PublicationRequest;
import org.folio.consortia.domain.dto.PublicationResponse;
import org.folio.consortia.domain.dto.PublicationStatus;
import org.folio.consortia.domain.dto.SharingRoleRequest;
import org.folio.consortia.domain.dto.Tenant;
import org.folio.consortia.domain.dto.TenantCollection;
import org.folio.consortia.domain.entity.SharingRoleEntity;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.ConsortiumRepository;
import org.folio.consortia.repository.PublicationStatusRepository;
import org.folio.consortia.repository.SharingRoleRepository;
import org.folio.consortia.service.impl.SharingRoleService;
import org.folio.spring.DefaultFolioExecutionContext;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.integration.XOkapiHeaders;
import org.folio.spring.scope.FolioExecutionContextSetter;
import org.folio.spring.service.SystemUserScopedExecutionService;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.invocation.InvocationOnMock;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.task.TaskExecutor;
import org.springframework.http.HttpMethod;
import org.springframework.test.util.ReflectionTestUtils;

@SpringBootTest
class SharingRoleServiceTest {
  private static final String SHARING_ROLE_REQUEST_SAMPLE = "mockdata/sharing_roles/sharing_role_request.json";
  private static final String SHARING_ROLE_REQUEST_SAMPLE_WITHOUT_PAYLOAD = "mockdata/sharing_roles/sharing_role_request_without_payload.json";

  @InjectMocks
  private SharingRoleService sharingRoleService;
  @Mock
  private ConsortiumRepository consortiumRepository;
  @Mock
  private ConsortiumService consortiumService;
  @Mock
  private TaskExecutor asyncTaskExecutor;
  @Mock
  private TenantService tenantService;
  @Mock
  private PublicationService publicationService;
  @Mock
  private PublicationStatusRepository publicationStatusRepository;
  @Mock
  private SharingRoleRepository sharingRoleRepository;
  @Mock
  private FolioExecutionContext folioExecutionContext;
  @Mock
  private SystemUserScopedExecutionService systemUserScopedExecutionService;
  @Mock
  private ObjectMapper objectMapper;

  @Test
  void shouldStartSharingRole() throws JsonProcessingException {
    UUID createRolesPcId = UUID.randomUUID();
    UUID updateRolesPcId = UUID.randomUUID();
    Tenant tenant1 = createTenant("tenant1", "tenant1");
    Tenant tenant2 = createTenant("tenant2", "tenant2");
    Set<String> tenantAssociationsWithRole = Set.of("tenant1");
    TenantCollection tenantCollection = createTenantCollection(List.of(tenant1, tenant2));
    var sharingRoleRequest = getMockDataObject(SHARING_ROLE_REQUEST_SAMPLE, SharingRoleRequest.class);
    Map<String, String> payload = new LinkedHashMap<>();
    payload.put("id", "3844767a-8367-4926-9999-514c35840399");
    payload.put("name", "Role for policy: 104d7a66-c51d-402a-9c9f-3bdcdbbcdbe7");
    payload.put("type", "local");

    // "tenant1" exists in tenant role association so that tenant1 is in PUT request publication,
    // "tenant2" is in POST method publication
    var publicationRequestPut = createPublicationRequest(sharingRoleRequest, HttpMethod.PUT.toString());
    publicationRequestPut.setMethod("PUT");
    publicationRequestPut.setTenants(Set.of("tenant1"));
    publicationRequestPut.setUrl("/role/3844767a-8367-4926-9999-514c35840399");
    var publicationRequestPost = createPublicationRequest(sharingRoleRequest, HttpMethod.POST.toString());
    publicationRequestPost.setMethod("POST");
    publicationRequestPost.setTenants(Set.of("tenant2"));

    var publicationResponsePost = new PublicationResponse().id(createRolesPcId);
    var publicationResponsePut = new PublicationResponse().id(updateRolesPcId);

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);
    when(publicationService.publishRequest(CONSORTIUM_ID, publicationRequestPost)).thenReturn(publicationResponsePost);
    when(publicationService.publishRequest(CONSORTIUM_ID, publicationRequestPut)).thenReturn(publicationResponsePut);
    when(tenantService.getAll(CONSORTIUM_ID)).thenReturn(tenantCollection);
    when(sharingRoleRepository.findTenantsByRoleId(sharingRoleRequest.getRoleId())).thenReturn(tenantAssociationsWithRole);
    when(sharingRoleRepository.save(any())).thenReturn(new SharingRoleEntity());
    when(folioExecutionContext.getTenantId()).thenReturn("mobius");
    when(systemUserScopedExecutionService.executeSystemUserScoped(eq("mobius"), any())).then(SharingRoleServiceTest::callSecondArgument);
    when(objectMapper.convertValue(payload, JsonNode.class)).thenReturn(createJsonNodeForRolePayload());

    var expectedResponse = createSharingRoleResponse(createRolesPcId, updateRolesPcId);
    var actualResponse = sharingRoleService.start(CONSORTIUM_ID, sharingRoleRequest);

    assertThat(actualResponse.getCreateRolesPCId()).isEqualTo(expectedResponse.getCreateRolesPCId());
    assertThat(actualResponse.getUpdateRolesPCId()).isEqualTo(expectedResponse.getUpdateRolesPCId());

    verify(publicationService, times(2)).publishRequest(any(), any());
  }

  @Test
  void shouldDeleteSharingRole() {
    UUID pcId = UUID.randomUUID();
    UUID roleId = UUID.fromString("3844767a-8367-4926-9999-514c35840399");
    Tenant tenant1 = createTenant("tenant1", "tenant1");
    Tenant tenant2 = createTenant("tenant2", "tenant2");
    Set<String> tenantAssociationsWithRole = Set.of("tenant1");
    TenantCollection tenantCollection = createTenantCollection(List.of(tenant1, tenant2));
    var sharingRoleRequest = getMockDataObject(SHARING_ROLE_REQUEST_SAMPLE, SharingRoleRequest.class);

    // "tenant1" exists in tenant role association so that tenant1 is in DELETE request publication,
    var expectedPublicationRequestDelete = createPublicationRequest(sharingRoleRequest, HttpMethod.DELETE.toString());
    expectedPublicationRequestDelete.setTenants(Set.of("tenant1"));
    expectedPublicationRequestDelete.setUrl("/role/3844767a-8367-4926-9999-514c35840399");
    Map<String, String> map = new LinkedHashMap<>();
    map.put("id", "3844767a-8367-4926-9999-514c35840399");
    map.put("name", "Role for policy: 104d7a66-c51d-402a-9c9f-3bdcdbbcdbe7");
    map.put("type", "local");
    expectedPublicationRequestDelete.setPayload(map);

    var publicationResponse = new PublicationResponse().id(pcId);

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);
    when(sharingRoleRepository.existsByRoleId(roleId)).thenReturn(true);
    when(publicationService.publishRequest(CONSORTIUM_ID, expectedPublicationRequestDelete)).thenReturn(publicationResponse);
    when(tenantService.getAll(CONSORTIUM_ID)).thenReturn(tenantCollection);
    when(sharingRoleRepository.findTenantsByRoleId(sharingRoleRequest.getRoleId())).thenReturn(tenantAssociationsWithRole);
    when(folioExecutionContext.getTenantId()).thenReturn("mobius");
    when(systemUserScopedExecutionService.executeSystemUserScoped(eq("mobius"), any())).then(SharingRoleServiceTest::callSecondArgument);

    var expectedResponse = createSharingRoleResponseForDelete(pcId);
    var actualResponse = sharingRoleService.delete(CONSORTIUM_ID, roleId, sharingRoleRequest);

    assertThat(actualResponse.getPcId()).isEqualTo(expectedResponse.getPcId());

    verify(publicationService, times(1)).publishRequest(any(), any());
  }

  @Test
  void shouldUpdateFailedTenantPolicies() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, JsonProcessingException {
    UUID publicationId = UUID.randomUUID();
    UUID pcId = UUID.randomUUID();
    var publicationResponse = new PublicationResponse().id(pcId);
    var sharingRoleRequest = getMockDataObject(SHARING_ROLE_REQUEST_SAMPLE, SharingRoleRequest.class);
    String centralTenant = "mobius";
    String localTenant = "school";
    var publicationResultCollection = createPublicationResultCollection(centralTenant, localTenant);
    var publicationDetails = createPublicationDetails(PublicationStatus.ERROR);
    JsonNode node = createJsonNodeForGroupPayload();
    // expected data for publish request
    Set<String> expectedFailedTenantList = new HashSet<>(List.of(centralTenant, localTenant));
    var expectedPublicationRequest = createExceptedPublicationRequest(sharingRoleRequest, expectedFailedTenantList, HttpMethod.PUT);

    // set time interval and maxTries for Thread sleep cycle
    ReflectionTestUtils.setField(sharingRoleService, "maxTries", 60);
    ReflectionTestUtils.setField(sharingRoleService, "interval", 200);

    when(publicationService.checkPublicationDetailsExists(CONSORTIUM_ID, publicationId))
      .thenReturn(false)
      .thenReturn(false)
      .thenReturn(true);
    when(publicationService.getPublicationDetails(CONSORTIUM_ID, publicationId)).thenReturn(publicationDetails);
    when(publicationService.getPublicationResults(CONSORTIUM_ID, publicationId)).thenReturn(publicationResultCollection);
    when(objectMapper.convertValue(any(), eq(JsonNode.class))).thenReturn(node);
    when(folioExecutionContext.getTenantId()).thenReturn("mobius");
    when(systemUserScopedExecutionService.executeSystemUserScoped(eq("mobius"), any())).then(SharingRoleServiceTest::callSecondArgument);
    when(publicationService.publishRequest(CONSORTIUM_ID, expectedPublicationRequest)).thenReturn(publicationResponse);

    // Use reflection to access the protected method in BaseSharingService
    Method method = SharingRoleService.class.getSuperclass().getDeclaredMethod("updateConfigsForFailedTenantsWithRetry", UUID.class, UUID.class, Object.class);
    method.setAccessible(true);
    method.invoke(sharingRoleService, CONSORTIUM_ID, publicationId, sharingRoleRequest);

    verify(publicationService).getPublicationDetails(CONSORTIUM_ID, publicationId);
    verify(publicationService, times(3)).checkPublicationDetailsExists(CONSORTIUM_ID, publicationId);
    verify(publicationService).publishRequest(CONSORTIUM_ID, expectedPublicationRequest);
  }

  // Negative cases
  @Test
  void shouldThrowErrorForNotEqualRoleIdWithPayloadId() throws JsonProcessingException {
    var sharingRoleRequest = getMockDataObject(SHARING_ROLE_REQUEST_SAMPLE, SharingRoleRequest.class);
    sharingRoleRequest.setRoleId(UUID.randomUUID());
    JsonNode node = createJsonNodeForRolePayload();

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);
    when(objectMapper.convertValue(any(), eq(JsonNode.class))).thenReturn(node);

    assertThrows(IllegalArgumentException.class, () -> sharingRoleService.start(CONSORTIUM_ID, sharingRoleRequest));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotEqualRoleIdPathId() {
    UUID roleId = UUID.fromString("999999-8367-4926-9999-514c35840399");

    var sharingRoleRequest = getMockDataObject(SHARING_ROLE_REQUEST_SAMPLE, SharingRoleRequest.class);

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);

    assertThrows(IllegalArgumentException.class,
      () -> sharingRoleService.delete(CONSORTIUM_ID, roleId, sharingRoleRequest));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotHavingPayloadOfRole() {
    var sharingRoleRequest = getMockDataObject(SHARING_ROLE_REQUEST_SAMPLE_WITHOUT_PAYLOAD, SharingRoleRequest.class);

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);

    assertThrows(IllegalArgumentException.class,
      () -> sharingRoleService.delete(CONSORTIUM_ID, sharingRoleRequest.getRoleId(), sharingRoleRequest));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotFound() {
    UUID roleId = UUID.fromString("3844767a-8367-4926-9999-514c35840399");

    var sharingRoleRequest = getMockDataObject(SHARING_ROLE_REQUEST_SAMPLE, SharingRoleRequest.class);

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);
    when(sharingRoleRepository.existsByRoleId(roleId)).thenReturn(false);

    assertThrows(ResourceNotFoundException.class,
      () -> sharingRoleService.delete(CONSORTIUM_ID, roleId, sharingRoleRequest));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  public static PublicationRequest createExceptedPublicationRequest(SharingRoleRequest sharingRoleRequest, Set<String> tenantList, HttpMethod method) {
    var expectedPublicationRequest = new PublicationRequest();
    expectedPublicationRequest.setTenants(tenantList);
    expectedPublicationRequest.setMethod(method.toString());
    expectedPublicationRequest.setUrl(sharingRoleRequest.getUrl() + "/" + sharingRoleRequest.getRoleId());
    final ObjectMapper mapper = new ObjectMapper();
    final ObjectNode root = mapper.createObjectNode();
    root.set("group", mapper.convertValue("space", JsonNode.class));
    root.set("type", mapper.convertValue("user", JsonNode.class));
    expectedPublicationRequest.setPayload(root);
    return expectedPublicationRequest;
  }

  private static <T> T callSecondArgument(InvocationOnMock invocation) throws Exception {
    var headers = Map.<String, Collection<String>>of(XOkapiHeaders.TENANT, List.of("mobius"));
    var context = new DefaultFolioExecutionContext(mock(FolioModuleMetadata.class), headers);
    try(var ignored = new FolioExecutionContextSetter(context)) {
      return invocation.<Callable<T>>getArgument(1).call();
    }
  }
}
