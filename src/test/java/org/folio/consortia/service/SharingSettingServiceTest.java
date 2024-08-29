package org.folio.consortia.service;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.folio.consortia.support.EntityUtils.createPublicationRequest;
import static org.folio.consortia.support.TestConstants.CONSORTIUM_ID;
import static org.folio.consortia.support.EntityUtils.createJsonNodeForDepartmentPayload;
import static org.folio.consortia.support.EntityUtils.createJsonNodeForGroupPayload;
import static org.folio.consortia.support.EntityUtils.createPublicationDetails;
import static org.folio.consortia.support.EntityUtils.createPublicationResultCollection;
import static org.folio.consortia.support.EntityUtils.createSharingSettingResponse;
import static org.folio.consortia.support.EntityUtils.createSharingSettingResponseForDelete;
import static org.folio.consortia.support.EntityUtils.createTenant;
import static org.folio.consortia.support.EntityUtils.createTenantCollection;
import static org.folio.consortia.utils.InputOutputTestUtils.getMockDataObject;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collection;
import java.util.concurrent.Callable;
import org.folio.consortia.domain.entity.SharingSettingEntity;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.ConsortiumRepository;
import org.folio.consortia.repository.PublicationStatusRepository;
import org.folio.consortia.repository.SharingSettingRepository;
import org.folio.consortia.service.impl.SharingSettingService;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import org.folio.consortia.domain.dto.PublicationRequest;
import org.folio.consortia.domain.dto.PublicationResponse;
import org.folio.consortia.domain.dto.PublicationStatus;
import org.folio.consortia.domain.dto.SharingSettingRequest;
import org.folio.consortia.domain.dto.Tenant;
import org.folio.consortia.domain.dto.TenantCollection;
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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

@SpringBootTest
class SharingSettingServiceTest {
  private static final String SHARING_SETTING_REQUEST_SAMPLE_FOR_DEPARTMENT = "mockdata/sharing_settings/sharing_setting_request_for_department.json";
  private static final String SHARING_SETTING_REQUEST_SAMPLE_FOR_GROUP = "mockdata/sharing_settings/sharing_setting_request_for_group.json";
  private static final String SHARING_SETTING_REQUEST_SAMPLE_WITHOUT_PAYLOAD = "mockdata/sharing_settings/sharing_setting_request_without_payload.json";

  @InjectMocks
  private SharingSettingService sharingSettingService;
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
  private SharingSettingRepository sharingSettingRepository;
  @Mock
  private FolioExecutionContext folioExecutionContext;
  @Mock
  private SystemUserScopedExecutionService systemUserScopedExecutionService;
  @Mock
  private ObjectMapper objectMapper;

  @Test
  void shouldStartSharingSetting() {
    UUID createSettingsPcId = UUID.randomUUID();
    UUID updateSettingsPcId = UUID.randomUUID();
    Tenant tenant1 = createTenant("tenant1", "tenant1");
    Tenant tenant2 = createTenant("tenant2", "tenant2");
    Set<String> tenantAssociationsWithSetting = Set.of("tenant1");
    TenantCollection tenantCollection = createTenantCollection(List.of(tenant1, tenant2));
    var sharingSettingRequest = getMockDataObject(SHARING_SETTING_REQUEST_SAMPLE_FOR_DEPARTMENT, SharingSettingRequest.class);
    Map<String, String> payload = new LinkedHashMap<>();
    payload.put("id", "1844767a-8367-4926-9999-514c35840399");
    payload.put("name", "ORG-NAME");
    payload.put("source", "local");

    // "tenant1" exists in tenant setting association so that tenant1 is in PUT request publication,
    // "tenant2" is in POST method publication
    var publicationRequestPut = createPublicationRequest(sharingSettingRequest, HttpMethod.PUT.toString());
    publicationRequestPut.setMethod("PUT");
    publicationRequestPut.setTenants(Set.of("tenant1"));
    publicationRequestPut.setUrl("/organizations-storage/organizations/1844767a-8367-4926-9999-514c35840399");
    var publicationRequestPost = createPublicationRequest(sharingSettingRequest, HttpMethod.POST.toString());
    publicationRequestPost.setMethod("POST");
    publicationRequestPost.setTenants(Set.of("tenant2"));

    var publicationResponsePost = new PublicationResponse().id(createSettingsPcId);
    var publicationResponsePut = new PublicationResponse().id(updateSettingsPcId);

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);
    when(publicationService.publishRequest(CONSORTIUM_ID, publicationRequestPost)).thenReturn(publicationResponsePost);
    when(publicationService.publishRequest(CONSORTIUM_ID, publicationRequestPut)).thenReturn(publicationResponsePut);
    when(tenantService.getAll(CONSORTIUM_ID)).thenReturn(tenantCollection);
    when(sharingSettingRepository.findTenantsBySettingId(sharingSettingRequest.getSettingId())).thenReturn(tenantAssociationsWithSetting);
    when(sharingSettingRepository.save(any())).thenReturn(new SharingSettingEntity());
    when(folioExecutionContext.getTenantId()).thenReturn("mobius");
    when(systemUserScopedExecutionService.executeSystemUserScoped(eq("mobius"), any())).then(SharingSettingServiceTest::callSecondArgument);
    when(objectMapper.convertValue(payload, ObjectNode.class)).thenReturn(createJsonNodeForDepartmentPayload());

    var expectedResponse = createSharingSettingResponse(createSettingsPcId, updateSettingsPcId);
    var actualResponse = sharingSettingService.start(CONSORTIUM_ID, sharingSettingRequest);

    assertThat(actualResponse.getCreateSettingsPCId()).isEqualTo(expectedResponse.getCreateSettingsPCId());
    assertThat(actualResponse.getUpdateSettingsPCId()).isEqualTo(expectedResponse.getUpdateSettingsPCId());

    verify(publicationService, times(2)).publishRequest(any(), any());
  }

  @Test
  void shouldDeleteSharingSetting() {
    UUID pcId = UUID.randomUUID();
    UUID settingId = UUID.fromString("1844767a-8367-4926-9999-514c35840399");
    Tenant tenant1 = createTenant("tenant1", "tenant1");
    Tenant tenant2 = createTenant("tenant2", "tenant2");
    Set<String> tenantAssociationsWithSetting = Set.of("tenant1");
    TenantCollection tenantCollection = createTenantCollection(List.of(tenant1, tenant2));
    var sharingSettingRequest = getMockDataObject(SHARING_SETTING_REQUEST_SAMPLE_FOR_DEPARTMENT, SharingSettingRequest.class);

    // "tenant1" exists in tenant setting association so that tenant1 is in DELETE request publication,
    var expectedPublicationRequestDelete = createPublicationRequest(sharingSettingRequest, HttpMethod.DELETE.toString());
    expectedPublicationRequestDelete.setTenants(Set.of("tenant1"));
    expectedPublicationRequestDelete.setUrl("/organizations-storage/organizations/1844767a-8367-4926-9999-514c35840399");
    Map<String, String> map = new LinkedHashMap<>();
    map.put("id", "1844767a-8367-4926-9999-514c35840399");
    map.put("name", "ORG-NAME");
    map.put("source", "local");
    expectedPublicationRequestDelete.setPayload(map);

    var publicationResponse = new PublicationResponse().id(pcId);

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);
    when(sharingSettingRepository.existsBySettingId(settingId)).thenReturn(true);
    when(publicationService.publishRequest(CONSORTIUM_ID, expectedPublicationRequestDelete)).thenReturn(publicationResponse);
    when(tenantService.getAll(CONSORTIUM_ID)).thenReturn(tenantCollection);
    when(sharingSettingRepository.findTenantsBySettingId(sharingSettingRequest.getSettingId())).thenReturn(tenantAssociationsWithSetting);
    when(folioExecutionContext.getTenantId()).thenReturn("mobius");
    when(systemUserScopedExecutionService.executeSystemUserScoped(eq("mobius"), any())).then(SharingSettingServiceTest::callSecondArgument);

    var expectedResponse = createSharingSettingResponseForDelete(pcId);
    var actualResponse = sharingSettingService.delete(CONSORTIUM_ID, settingId, sharingSettingRequest);

    assertThat(actualResponse.getPcId()).isEqualTo(expectedResponse.getPcId());

    verify(publicationService, times(1)).publishRequest(any(), any());
  }

  @Test
  void shouldUpdateFailedTenantSettings() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
    UUID publicationId = UUID.randomUUID();
    UUID pcId = UUID.randomUUID();
    var publicationResponse = new PublicationResponse().id(pcId);
    var sharingSettingRequest = getMockDataObject(SHARING_SETTING_REQUEST_SAMPLE_FOR_GROUP, SharingSettingRequest.class);
    String centralTenant = "mobius";
    String localTenant = "school";
    var publicationResultCollection = createPublicationResultCollection(centralTenant, localTenant);
    var publicationDetails = createPublicationDetails(PublicationStatus.ERROR);
    var node = createJsonNodeForGroupPayload();
    // expected data for publish request
    Set<String> expectedFailedTenantList = new HashSet<>(List.of(centralTenant, localTenant));
    var expectedPublicationRequest = createExceptedPublicationRequest(sharingSettingRequest, expectedFailedTenantList, HttpMethod.PUT);

    // set time interval and maxTries for Thread sleep cycle
    ReflectionTestUtils.setField(sharingSettingService, "maxTries", 60);
    ReflectionTestUtils.setField(sharingSettingService, "interval", 200);

    when(publicationService.checkPublicationDetailsExists(CONSORTIUM_ID, publicationId))
      .thenReturn(false)
      .thenReturn(false)
      .thenReturn(true);
    when(publicationService.getPublicationDetails(CONSORTIUM_ID, publicationId)).thenReturn(publicationDetails);
    when(publicationService.getPublicationResults(CONSORTIUM_ID, publicationId)).thenReturn(publicationResultCollection);
    when(objectMapper.convertValue(any(), eq(ObjectNode.class))).thenReturn(node);
    when(folioExecutionContext.getTenantId()).thenReturn("mobius");
    when(systemUserScopedExecutionService.executeSystemUserScoped(eq("mobius"), any())).then(SharingSettingServiceTest::callSecondArgument);
    when(publicationService.publishRequest(CONSORTIUM_ID, expectedPublicationRequest)).thenReturn(publicationResponse);

    // Use reflection to access the protected method in BaseSharingService
    Method method = SharingSettingService.class.getSuperclass().getDeclaredMethod("updateConfigsForFailedTenantsWithRetry", UUID.class, UUID.class, Object.class);
    method.setAccessible(true);
    method.invoke(sharingSettingService, CONSORTIUM_ID, publicationId, sharingSettingRequest);

    verify(publicationService).getPublicationDetails(CONSORTIUM_ID, publicationId);
    verify(publicationService, times(3)).checkPublicationDetailsExists(CONSORTIUM_ID, publicationId);
    verify(publicationService).publishRequest(CONSORTIUM_ID, expectedPublicationRequest);
  }

  // Negative cases
  @Test
  void shouldThrowErrorForNotEqualSettingIdWithPayloadId() {
    var sharingSettingRequest = getMockDataObject(SHARING_SETTING_REQUEST_SAMPLE_FOR_DEPARTMENT, SharingSettingRequest.class);
    sharingSettingRequest.setSettingId(UUID.randomUUID());
    var node = createJsonNodeForDepartmentPayload();

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);
    when(objectMapper.convertValue(any(), eq(ObjectNode.class))).thenReturn(node);

    assertThrows(java.lang.IllegalArgumentException.class, () -> sharingSettingService.start(CONSORTIUM_ID, sharingSettingRequest));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotEqualSettingIdPathId() {
    UUID settingId = UUID.fromString("999999-8367-4926-9999-514c35840399");

    var sharingSettingRequest = getMockDataObject(SHARING_SETTING_REQUEST_SAMPLE_FOR_GROUP, SharingSettingRequest.class);

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);

    assertThrows(java.lang.IllegalArgumentException.class,
      () -> sharingSettingService.delete(CONSORTIUM_ID, settingId, sharingSettingRequest));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotHavingPayloadOfSetting() {
    var sharingSettingRequest = getMockDataObject(SHARING_SETTING_REQUEST_SAMPLE_WITHOUT_PAYLOAD, SharingSettingRequest.class);

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);

    assertThrows(java.lang.IllegalArgumentException.class,
      () -> sharingSettingService.delete(CONSORTIUM_ID, sharingSettingRequest.getSettingId(), sharingSettingRequest));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotFound() {
    UUID settingId = UUID.fromString("1844767a-8367-4926-9999-514c35840399");

    var sharingSettingRequest = getMockDataObject(SHARING_SETTING_REQUEST_SAMPLE_FOR_DEPARTMENT, SharingSettingRequest.class);

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);
    when(sharingSettingRepository.existsBySettingId(settingId)).thenReturn(false);

    assertThrows(ResourceNotFoundException.class,
      () -> sharingSettingService.delete(CONSORTIUM_ID, settingId, sharingSettingRequest));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  private PublicationRequest createExceptedPublicationRequest(SharingSettingRequest sharingSettingRequest, Set<String> tenantList, HttpMethod method) {
    var expectedPublicationRequest = new PublicationRequest();
    expectedPublicationRequest.setTenants(tenantList);
    expectedPublicationRequest.setMethod(method.toString());
    expectedPublicationRequest.setUrl(sharingSettingRequest.getUrl() + "/" + sharingSettingRequest.getSettingId());
    final ObjectMapper mapper = new ObjectMapper();
    final ObjectNode root = mapper.createObjectNode();
    root.set("group", mapper.convertValue("space", JsonNode.class));
    root.set("source", mapper.convertValue("user", JsonNode.class));
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
