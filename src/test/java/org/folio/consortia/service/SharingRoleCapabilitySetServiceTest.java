package org.folio.consortia.service;

import com.fasterxml.jackson.databind.node.ObjectNode;
import org.folio.consortia.domain.dto.PublicationStatus;
import org.folio.consortia.domain.dto.SharingRoleCapabilitySetRequest;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.SharingRoleRepository;
import org.folio.consortia.service.impl.SharingRoleCapabilitySetService;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpMethod;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Set;
import java.util.UUID;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.folio.consortia.support.EntityUtils.SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE;
import static org.folio.consortia.support.EntityUtils.SHARING_ROLE_CAPABILITY_SETS_WITHOUT_PAYLOAD_REQUEST_SAMPLE;
import static org.folio.consortia.support.EntityUtils.TENANT_ID_1;
import static org.folio.consortia.support.EntityUtils.TENANT_ID_2;
import static org.folio.consortia.support.EntityUtils.createPayloadForRoleCapabilitySets;
import static org.folio.consortia.support.EntityUtils.createPublicationDetails;
import static org.folio.consortia.support.EntityUtils.createPublicationRequest;
import static org.folio.consortia.support.EntityUtils.createPublicationResultCollection;
import static org.folio.consortia.support.EntityUtils.createSharingRoleEntity;
import static org.folio.consortia.support.TestConstants.CENTRAL_TENANT_ID;
import static org.folio.consortia.support.TestConstants.CONSORTIUM_ID;
import static org.folio.consortia.utils.InputOutputTestUtils.getMockDataObject;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SpringBootTest
class SharingRoleCapabilitySetServiceTest extends BaseSharingConfigServiceTest{

  @InjectMocks
  SharingRoleCapabilitySetService sharingRoleCapabilitySetService;
  @Mock
  SharingRoleRepository sharingRoleRepository;

  @Test
  void shouldStartSharingRole() {
    var createPcId = UUID.randomUUID();
    var updatePcId = UUID.randomUUID();
    var tenantSharedRoleAndCapabilitySets = Set.of(TENANT_ID_1);
    var request = getMockDataObject(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE, SharingRoleCapabilitySetRequest.class);
    var sharingRoleEntity = createSharingRoleEntity(request.getRoleId(), TENANT_ID_2);
    var payload = createPayloadForRoleCapabilitySets();
    var expectedSharingRoleEntity = createSharingRoleEntity(request.getRoleId(), TENANT_ID_2);
    expectedSharingRoleEntity.setIsCapabilitySetsShared(true);

    // "tenant1" exists in tenant role association so that tenant1 is in PUT request publication,
    // "tenant2" is in POST method publication
    var expectedPubRequestPost = createPublicationRequest(payload, HttpMethod.POST)
      .tenants(Set.of(TENANT_ID_2))
      .url(request.getUrl());
    var expectedPubRequestPut = createPublicationRequest(payload, HttpMethod.PUT)
      .tenants(Set.of(TENANT_ID_1))
      .url("/roles/" + request.getRoleId() + "/capability-sets");

    setupCommonMocksForStart(createPcId, updatePcId, expectedPubRequestPost, expectedPubRequestPut, payload);
    when(sharingRoleRepository.findTenantsByRoleIdAndIsCapabilitySetsSharedTrue(request.getRoleId()))
      .thenReturn(tenantSharedRoleAndCapabilitySets);
    when(sharingRoleRepository.findByRoleIdAndTenantId(request.getRoleId(), TENANT_ID_2)).thenReturn(sharingRoleEntity);
    when(sharingRoleRepository.save(expectedSharingRoleEntity)).thenReturn(expectedSharingRoleEntity);

    var actualResponse = sharingRoleCapabilitySetService.start(CONSORTIUM_ID, request);

    assertThat(actualResponse.getCreateRoleCapabilitySetsPCId()).isEqualTo(createPcId);
    assertThat(actualResponse.getUpdateRoleCapabilitySetsPCId()).isEqualTo(updatePcId);

    verify(publicationService, times(2)).publishRequest(any(), any());
  }

  @Test
  void shouldDeleteSharingRole() {
    var pcId = UUID.randomUUID();
    var roleId = UUID.fromString("4844767a-8367-4926-9999-514c35840399");
    var tenantSharedRoleAndCapabilitySets = Set.of(TENANT_ID_1);
    var request = getMockDataObject(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE, SharingRoleCapabilitySetRequest.class);

    // "tenant1" exists in the tenant role association so that tenant1 is in DELETE request publication,
    var expectedPubRequestDelete = createPublicationRequest(HttpMethod.DELETE)
      .tenants(Set.of(TENANT_ID_1))
      .url("/roles/" + request.getRoleId() + "/capability-sets")
      .payload(request.getPayload());

    setupCommonMocksForDelete(pcId, expectedPubRequestDelete);
    when(sharingRoleRepository.existsByRoleId(roleId)).thenReturn(true);
    when(sharingRoleRepository.findTenantsByRoleIdAndIsCapabilitySetsSharedTrue(request.getRoleId()))
      .thenReturn(tenantSharedRoleAndCapabilitySets);

    var actualResponse = sharingRoleCapabilitySetService.delete(CONSORTIUM_ID, roleId, request);

    assertThat(actualResponse.getPcId()).isEqualTo(pcId);

    verify(publicationService, times(1)).publishRequest(any(), any());
  }

  @Test
  void shouldUpdateFailedTenantPolicies() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
    var publicationId = UUID.randomUUID();
    var pcId = UUID.randomUUID();
    var request = getMockDataObject(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE, SharingRoleCapabilitySetRequest.class);
    var pubResultCollection = createPublicationResultCollection(CENTRAL_TENANT_ID, TENANT_ID_2);
    var pubDetails = createPublicationDetails(PublicationStatus.ERROR);
    var payload = createPayloadForRoleCapabilitySets();

    // expected data for publish request
    var expectedPubRequest = createPublicationRequest(payload, HttpMethod.PUT)
      .url("/roles/" + request.getRoleId() + "/capability-sets")
      .tenants(Set.of(CENTRAL_TENANT_ID, TENANT_ID_2));

    setupCommonMocksForDelete(pcId, expectedPubRequest);
    when(publicationService.checkPublicationDetailsExists(CONSORTIUM_ID, publicationId))
      .thenReturn(false)
      .thenReturn(false)
      .thenReturn(true);
    when(publicationService.getPublicationDetails(CONSORTIUM_ID, publicationId)).thenReturn(pubDetails);
    when(publicationService.getPublicationResults(CONSORTIUM_ID, publicationId)).thenReturn(pubResultCollection);
    when(objectMapper.convertValue(any(), eq(ObjectNode.class))).thenReturn(payload);

    // Use reflection to access the protected method in BaseSharingService
    Method method = SharingRoleCapabilitySetService.class.getSuperclass()
      .getDeclaredMethod("updateConfigsForFailedTenantsWithRetry", UUID.class, UUID.class, Object.class);
    method.setAccessible(true);
    method.invoke(sharingRoleCapabilitySetService, CONSORTIUM_ID, publicationId, request);

    verify(publicationService).getPublicationDetails(CONSORTIUM_ID, publicationId);
    verify(publicationService, times(3)).checkPublicationDetailsExists(CONSORTIUM_ID, publicationId);
    verify(publicationService).publishRequest(CONSORTIUM_ID, expectedPubRequest);
  }

  @Test
  void shouldThrowErrorForNotEqualRoleIdWithPayloadId() {
    var request = getMockDataObject(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE, SharingRoleCapabilitySetRequest.class);
    request.setRoleId(UUID.randomUUID());
    var payload = createPayloadForRoleCapabilitySets();

    when(objectMapper.convertValue(any(), eq(ObjectNode.class))).thenReturn(payload);

    assertThrows(java.lang.IllegalArgumentException.class,
      () -> sharingRoleCapabilitySetService.start(CONSORTIUM_ID, request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotEqualRoleIdPathId() {
    UUID roleId = UUID.fromString("999999-8367-4926-9999-514c35840399");
    var request = getMockDataObject(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE,
      SharingRoleCapabilitySetRequest.class);

    assertThrows(IllegalArgumentException.class,
      () -> sharingRoleCapabilitySetService.delete(CONSORTIUM_ID, roleId, request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotHavingPayloadOfRole() {
    var request = getMockDataObject(SHARING_ROLE_CAPABILITY_SETS_WITHOUT_PAYLOAD_REQUEST_SAMPLE,
      SharingRoleCapabilitySetRequest.class);

    assertThrows(java.lang.IllegalArgumentException.class,
      () -> sharingRoleCapabilitySetService.delete(CONSORTIUM_ID, request.getRoleId(), request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotFound() {
    UUID roleId = UUID.fromString("4844767a-8367-4926-9999-514c35840399");
    var request = getMockDataObject(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE, SharingRoleCapabilitySetRequest.class);

    when(sharingRoleRepository.existsByRoleId(roleId)).thenReturn(false);

    assertThrows(ResourceNotFoundException.class,
      () -> sharingRoleCapabilitySetService.delete(CONSORTIUM_ID, roleId, request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Override
  protected Object getServiceUnderTest() {
    return sharingRoleCapabilitySetService;
  }
}
