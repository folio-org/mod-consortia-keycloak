package org.folio.consortia.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.folio.consortia.domain.dto.PublicationResponse;
import org.folio.consortia.domain.dto.PublicationStatus;
import org.folio.consortia.domain.dto.SharingRoleCapabilitySetRequest;
import org.folio.consortia.domain.entity.SharingRoleEntity;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.ConsortiumRepository;
import org.folio.consortia.repository.PublicationStatusRepository;
import org.folio.consortia.repository.SharingRoleRepository;
import org.folio.consortia.service.impl.SharingRoleCapabilitySetService;
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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Callable;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.folio.consortia.support.EntityUtils.SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE;
import static org.folio.consortia.support.EntityUtils.SHARING_ROLE_CAPABILITY_SETS_WITHOUT_PAYLOAD_REQUEST_SAMPLE;
import static org.folio.consortia.support.EntityUtils.TENANT_ID_1;
import static org.folio.consortia.support.EntityUtils.TENANT_ID_2;
import static org.folio.consortia.support.EntityUtils.createExceptedPublicationRequest;
import static org.folio.consortia.support.EntityUtils.createJsonNodeForGroupPayload;
import static org.folio.consortia.support.EntityUtils.createJsonNodeForRoleCapabilitySetsPayload;
import static org.folio.consortia.support.EntityUtils.createPublicationDetails;
import static org.folio.consortia.support.EntityUtils.createPublicationRequest;
import static org.folio.consortia.support.EntityUtils.createPublicationResultCollection;
import static org.folio.consortia.support.EntityUtils.createSharingRoleCapabilitySetResponse;
import static org.folio.consortia.support.EntityUtils.createSharingRoleEntity;
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

@SpringBootTest
class SharingRoleCapabilitySetServiceTest {

  @InjectMocks
  private SharingRoleCapabilitySetService sharingRoleCapabilitySetService;
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
  void shouldStartSharingRole() {
    var createPcId = UUID.randomUUID();
    var updatePcId = UUID.randomUUID();
    var tenant1 = createTenant(TENANT_ID_1);
    var tenant2 = createTenant(TENANT_ID_2);
    var tenantAssociationsWithRole = Set.of(TENANT_ID_1);
    var tenantCollection = createTenantCollection(List.of(tenant1, tenant2));
    var request = getMockDataObject(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE, SharingRoleCapabilitySetRequest.class);
    var sharingRoleEntity = createSharingRoleEntity(request.getRoleId(), TENANT_ID_2);
    var expectedSharingRoleEntity = createSharingRoleEntity(request.getRoleId(), TENANT_ID_2);
    expectedSharingRoleEntity.setIsCapabilitySetsShared(true);

    // "tenant1" exists in tenant role association so that tenant1 is in PUT request publication,
    // "tenant2" is in POST method publication
    var publicationRequestPut = createPublicationRequest(request, HttpMethod.PUT.toString())
      .tenants(Set.of(TENANT_ID_1))
      .url("/role/4844767a-8367-4926-9999-514c35840399/capability-sets");
    var publicationRequestPost = createPublicationRequest(request, HttpMethod.POST.toString())
      .tenants(Set.of(TENANT_ID_2));

    var publicationResponsePost = new PublicationResponse().id(createPcId);
    var publicationResponsePut = new PublicationResponse().id(updatePcId);

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);
    when(publicationService.publishRequest(CONSORTIUM_ID, publicationRequestPost))
      .thenReturn(publicationResponsePost);
    when(publicationService.publishRequest(CONSORTIUM_ID, publicationRequestPut))
      .thenReturn(publicationResponsePut);
    when(tenantService.getAll(CONSORTIUM_ID)).thenReturn(tenantCollection);
    when(sharingRoleRepository.findTenantsByRoleIdAndSharedCapabilitySets(request.getRoleId()))
      .thenReturn(tenantAssociationsWithRole);
    when(sharingRoleRepository.findByRoleIdAndTenantId(request.getRoleId(), TENANT_ID_2))
      .thenReturn(sharingRoleEntity);
    when(sharingRoleRepository.save(expectedSharingRoleEntity)).thenReturn(new SharingRoleEntity());
    when(folioExecutionContext.getTenantId()).thenReturn("mobius");
    when(systemUserScopedExecutionService.executeSystemUserScoped(eq("mobius"), any()))
      .then(SharingRoleCapabilitySetServiceTest::callSecondArgument);
    when(objectMapper.convertValue(request.getPayload(), ObjectNode.class))
      .thenReturn(createJsonNodeForRoleCapabilitySetsPayload());

    var expectedResponse = createSharingRoleCapabilitySetResponse(createPcId, updatePcId);
    var actualResponse = sharingRoleCapabilitySetService.start(CONSORTIUM_ID, request);

    assertThat(actualResponse.getCreateRoleCapabilitySetsPCId())
      .isEqualTo(expectedResponse.getCreateRoleCapabilitySetsPCId());
    assertThat(actualResponse.getUpdateRoleCapabilitySetsPCId())
      .isEqualTo(expectedResponse.getUpdateRoleCapabilitySetsPCId());

    verify(publicationService, times(2)).publishRequest(any(), any());
  }

  @Test
  void shouldDeleteSharingRole() {
    var pcId = UUID.randomUUID();
    var roleId = UUID.fromString("4844767a-8367-4926-9999-514c35840399");
    var tenant1 = createTenant(TENANT_ID_1);
    var tenant2 = createTenant(TENANT_ID_2);
    var tenantAssociationsWithRole = Set.of(TENANT_ID_1);
    var tenantCollection = createTenantCollection(List.of(tenant1, tenant2));
    var request = getMockDataObject(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE, SharingRoleCapabilitySetRequest.class);

    // "tenant1" exists in the tenant role association so that tenant1 is in DELETE request publication,
    var expectedPublicationRequestDelete = createPublicationRequest(request, HttpMethod.DELETE.toString())
      .tenants(Set.of(TENANT_ID_1))
      .url("/role/4844767a-8367-4926-9999-514c35840399/capability-sets")
      .payload(request.getPayload());

    var publicationResponse = new PublicationResponse().id(pcId);

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);
    when(sharingRoleRepository.existsByRoleId(roleId)).thenReturn(true);
    when(publicationService.publishRequest(CONSORTIUM_ID, expectedPublicationRequestDelete))
      .thenReturn(publicationResponse);
    when(tenantService.getAll(CONSORTIUM_ID)).thenReturn(tenantCollection);
    when(sharingRoleRepository.findTenantsByRoleIdAndSharedCapabilitySets(request.getRoleId()))
      .thenReturn(tenantAssociationsWithRole);
    when(folioExecutionContext.getTenantId()).thenReturn("mobius");
    when(systemUserScopedExecutionService.executeSystemUserScoped(eq("mobius"), any()))
      .then(SharingRoleCapabilitySetServiceTest::callSecondArgument);

    var expectedResponse = createSharingRoleResponseForDelete(pcId);
    var actualResponse = sharingRoleCapabilitySetService.delete(CONSORTIUM_ID, roleId, request);

    assertThat(actualResponse.getPcId()).isEqualTo(expectedResponse.getPcId());

    verify(publicationService, times(1)).publishRequest(any(), any());
  }

  @Test
  void shouldUpdateFailedTenantPolicies() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
    var publicationId = UUID.randomUUID();
    var pcId = UUID.randomUUID();
    var publicationResponse = new PublicationResponse().id(pcId);
    var request = getMockDataObject(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE, SharingRoleCapabilitySetRequest.class);
    var centralTenant = "mobius";
    var localTenant = "school";
    var publicationResultCollection = createPublicationResultCollection(centralTenant, localTenant);
    var publicationDetails = createPublicationDetails(PublicationStatus.ERROR);
    var node = createJsonNodeForGroupPayload();

    // expected data for publish request
    var expectedFailedTenantList = new HashSet<>(List.of(centralTenant, localTenant));
    var expectedPublicationRequest = createExceptedPublicationRequest(request, expectedFailedTenantList, HttpMethod.PUT);

    // set time interval and maxTries for Thread sleep cycle
    ReflectionTestUtils.setField(sharingRoleCapabilitySetService, "maxTries", 60);
    ReflectionTestUtils.setField(sharingRoleCapabilitySetService, "interval", 200);

    when(publicationService.checkPublicationDetailsExists(CONSORTIUM_ID, publicationId))
      .thenReturn(false)
      .thenReturn(false)
      .thenReturn(true);
    when(publicationService.getPublicationDetails(CONSORTIUM_ID, publicationId)).thenReturn(publicationDetails);
    when(publicationService.getPublicationResults(CONSORTIUM_ID, publicationId)).thenReturn(publicationResultCollection);
    when(objectMapper.convertValue(any(), eq(ObjectNode.class))).thenReturn(node);
    when(folioExecutionContext.getTenantId()).thenReturn("mobius");
    when(systemUserScopedExecutionService.executeSystemUserScoped(eq("mobius"), any())).then(SharingRoleCapabilitySetServiceTest::callSecondArgument);
    when(publicationService.publishRequest(CONSORTIUM_ID, expectedPublicationRequest)).thenReturn(publicationResponse);

    // Use reflection to access the protected method in BaseSharingService
    Method method = SharingRoleCapabilitySetService.class.getSuperclass()
      .getDeclaredMethod("updateConfigsForFailedTenantsWithRetry", UUID.class, UUID.class, Object.class);
    method.setAccessible(true);
    method.invoke(sharingRoleCapabilitySetService, CONSORTIUM_ID, publicationId, request);

    verify(publicationService).getPublicationDetails(CONSORTIUM_ID, publicationId);
    verify(publicationService, times(3)).checkPublicationDetailsExists(CONSORTIUM_ID, publicationId);
    verify(publicationService).publishRequest(CONSORTIUM_ID, expectedPublicationRequest);
  }

  // Negative cases
  @Test
  void shouldThrowErrorForNotEqualRoleIdWithPayloadId() {
    var request = getMockDataObject(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE, SharingRoleCapabilitySetRequest.class);
    request.setRoleId(UUID.randomUUID());
    var node = createJsonNodeForRoleCapabilitySetsPayload();

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);
    when(objectMapper.convertValue(any(), eq(ObjectNode.class))).thenReturn(node);

    assertThrows(IllegalArgumentException.class, () -> sharingRoleCapabilitySetService.start(CONSORTIUM_ID, request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotEqualRoleIdPathId() {
    UUID roleId = UUID.fromString("999999-8367-4926-9999-514c35840399");

    var request = getMockDataObject(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE, SharingRoleCapabilitySetRequest.class);

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);

    assertThrows(IllegalArgumentException.class,
      () -> sharingRoleCapabilitySetService.delete(CONSORTIUM_ID, roleId, request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotHavingPayloadOfRole() {
    var request = getMockDataObject(SHARING_ROLE_CAPABILITY_SETS_WITHOUT_PAYLOAD_REQUEST_SAMPLE,
      SharingRoleCapabilitySetRequest.class);

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);

    assertThrows(java.lang.IllegalArgumentException.class,
      () -> sharingRoleCapabilitySetService.delete(CONSORTIUM_ID, request.getRoleId(), request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotFound() {
    UUID roleId = UUID.fromString("4844767a-8367-4926-9999-514c35840399");

    var request = getMockDataObject(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE, SharingRoleCapabilitySetRequest.class);

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);
    when(sharingRoleRepository.existsByRoleId(roleId)).thenReturn(false);

    assertThrows(ResourceNotFoundException.class,
      () -> sharingRoleCapabilitySetService.delete(CONSORTIUM_ID, roleId, request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  private static <T> T callSecondArgument(InvocationOnMock invocation) throws Exception {
    var headers = Map.<String, Collection<String>>of(XOkapiHeaders.TENANT, List.of("mobius"));
    var context = new DefaultFolioExecutionContext(mock(FolioModuleMetadata.class), headers);
    try(var ignored = new FolioExecutionContextSetter(context)) {
      return invocation.<Callable<T>>getArgument(1).call();
    }
  }
}
