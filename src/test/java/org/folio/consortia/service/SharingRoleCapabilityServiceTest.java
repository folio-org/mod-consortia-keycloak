package org.folio.consortia.service;

import com.fasterxml.jackson.databind.node.ObjectNode;
import org.folio.consortia.domain.dto.PublicationStatus;
import org.folio.consortia.domain.dto.SharingRoleCapabilityRequest;
import org.folio.consortia.domain.entity.SharingRoleEntity;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.SharingRoleRepository;
import org.folio.consortia.service.impl.SharingRoleCapabilityService;
import org.folio.consortia.service.impl.SharingRoleCapabilitySetService;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpMethod;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.folio.consortia.domain.dto.SourceValues.CONSORTIUM;
import static org.folio.consortia.support.EntityUtils.SHARING_ROLE_CAPABILITIES_REQUEST_SAMPLE;
import static org.folio.consortia.support.EntityUtils.SHARING_ROLE_CAPABILITIES_WITHOUT_PAYLOAD_REQUEST_SAMPLE;
import static org.folio.consortia.support.EntityUtils.SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE;
import static org.folio.consortia.support.EntityUtils.TENANT_ID_1;
import static org.folio.consortia.support.EntityUtils.TENANT_ID_2;
import static org.folio.consortia.support.EntityUtils.createPayloadForRoleCapabilities;
import static org.folio.consortia.support.EntityUtils.createPublicationDetails;
import static org.folio.consortia.support.EntityUtils.createPublicationRequest;
import static org.folio.consortia.support.EntityUtils.createPublicationResultCollection;
import static org.folio.consortia.support.EntityUtils.createSharingRoleEntity;
import static org.folio.consortia.support.EntityUtils.createSharingRoleResponseForDelete;
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
class SharingRoleCapabilityServiceTest extends BaseSharingConfigServiceTest {

  @InjectMocks
  SharingRoleCapabilityService sharingRoleCapabilityService;
  @Mock
  SharingRoleRepository sharingRoleRepository;

  @Test
  @Disabled
  void shouldStartSharingRoleCapabilities() {
    var createPcId = UUID.randomUUID();
    var updatePcId = UUID.randomUUID();
    var tenantSharedRoleAndCapabilities = Set.of(TENANT_ID_1);
    var request = getMockDataObject(SHARING_ROLE_CAPABILITIES_REQUEST_SAMPLE, SharingRoleCapabilityRequest.class);
    var sharingRoleEntity = createSharingRoleEntity(request.getRoleId(), TENANT_ID_2);
    var payload = createPayloadForRoleCapabilities();
    var expectedSharingRoleEntity = createSharingRoleEntity(request.getRoleId(), TENANT_ID_2);
    expectedSharingRoleEntity.setIsCapabilitiesShared(true);

    // "tenant1" exists in tenant role association so that tenant1 is in PUT request publication,
    // "tenant2" is in POST method publication
    var expectedPubRequestPost = createPublicationRequest(CONSORTIUM.getRoleValue(), payload, HttpMethod.POST)
      .tenants(Set.of(TENANT_ID_2))
      .url(request.getUrl());
    var expectedPubRequestPut = createPublicationRequest(CONSORTIUM.getRoleValue(), payload, HttpMethod.PUT)
      .tenants(Set.of(TENANT_ID_1))
      .url("/roles/" + request.getRoleId() + "/capabilities");

    setupCommonMocksForStart(createPcId, updatePcId, expectedPubRequestPost, expectedPubRequestPut, payload);
    when(sharingRoleRepository.findTenantsByRoleNameAndIsCapabilitiesSharedTrue(request.getRoleName()))
      .thenReturn(tenantSharedRoleAndCapabilities);
    when(sharingRoleRepository.findByRoleNameAndTenantId(request.getRoleName(), TENANT_ID_2)).thenReturn(sharingRoleEntity);
    when(sharingRoleRepository.save(expectedSharingRoleEntity)).thenReturn(new SharingRoleEntity());

    var actualResponse = sharingRoleCapabilityService.start(CONSORTIUM_ID, request);

    assertThat(actualResponse.getCreatePCIds()).isEqualTo(createPcId);
    assertThat(actualResponse.getUpdatePCIds()).isEqualTo(updatePcId);

    verify(publicationService, times(2)).publishRequest(any(), any());
  }

  @Test
  @Disabled
  void shouldDeleteSharingRoleCapabilities() {
    var pcId = UUID.randomUUID();
    var roleId = UUID.fromString("5844767a-8367-4926-9999-514c35840399");
    var tenantSharedRoleAndCapabilities = Set.of(TENANT_ID_1);
    var request = getMockDataObject(SHARING_ROLE_CAPABILITIES_REQUEST_SAMPLE, SharingRoleCapabilityRequest.class);

    // "tenant1" exists in the tenant role association so that tenant1 is in DELETE request publication,
    var expectedPubRequestDelete = createPublicationRequest(HttpMethod.DELETE)
      .tenants(Set.of(TENANT_ID_1))
      .url("/roles/" + request.getRoleId() + "/capabilities")
      .payload(request.getPayload());

    setupCommonMocksForDelete(pcId, expectedPubRequestDelete);
    when(sharingRoleRepository.existsByRoleId(roleId)).thenReturn(true);
    when(sharingRoleRepository.findTenantsByRoleNameAndIsCapabilitiesSharedTrue(request.getRoleName()))
      .thenReturn(tenantSharedRoleAndCapabilities);

    var expectedResponse = createSharingRoleResponseForDelete(List.of(pcId));
    var actualResponse = sharingRoleCapabilityService.delete(CONSORTIUM_ID, roleId, request);

    assertThat(actualResponse.getPcIds()).isEqualTo(expectedResponse.getPcIds());
    verify(publicationService, times(1)).publishRequest(any(), any());
  }

  @Test
  @Disabled
  void shouldUpdateFailedTenantRoleCapabilities() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
    var publicationId = UUID.randomUUID();
    var pcId = UUID.randomUUID();
    var request = getMockDataObject(SHARING_ROLE_CAPABILITIES_REQUEST_SAMPLE, SharingRoleCapabilityRequest.class);
    var publicationResultCollection = createPublicationResultCollection(CENTRAL_TENANT_ID, TENANT_ID_2);
    var publicationDetails = createPublicationDetails(PublicationStatus.ERROR);
    var expectedPayload = createPayloadForRoleCapabilities();

    // expected data for publish request
    var expectedPublicationRequest = createPublicationRequest(CONSORTIUM.getRoleValue(), expectedPayload, HttpMethod.PUT)
      .url("/roles/" + request.getRoleId() + "/capabilities")
      .tenants(Set.of(CENTRAL_TENANT_ID, TENANT_ID_2));

    setupCommonMocksForDelete(pcId, expectedPublicationRequest);
    when(publicationService.checkPublicationDetailsExists(CONSORTIUM_ID, publicationId))
      .thenReturn(false)
      .thenReturn(false)
      .thenReturn(true);
    when(publicationService.getPublicationDetails(CONSORTIUM_ID, publicationId)).thenReturn(publicationDetails);
    when(publicationService.getPublicationResults(CONSORTIUM_ID, publicationId)).thenReturn(publicationResultCollection);
    when(objectMapper.convertValue(any(), eq(ObjectNode.class))).thenReturn(expectedPayload);

    // Use reflection to access the protected method in BaseSharingService
    Method method = SharingRoleCapabilitySetService.class.getSuperclass()
      .getDeclaredMethod("updateConfigsForFailedTenantsWithRetry", UUID.class, UUID.class, Object.class);
    method.setAccessible(true);
    method.invoke(sharingRoleCapabilityService, CONSORTIUM_ID, publicationId, request);

    verify(publicationService).getPublicationDetails(CONSORTIUM_ID, publicationId);
    verify(publicationService, times(3)).checkPublicationDetailsExists(CONSORTIUM_ID, publicationId);
    verify(publicationService).publishRequest(CONSORTIUM_ID, expectedPublicationRequest);
  }

  // Negative cases
  @Test
  void shouldThrowErrorForNotEqualRoleIdWithPayloadId() {
    var request = getMockDataObject(SHARING_ROLE_CAPABILITIES_REQUEST_SAMPLE, SharingRoleCapabilityRequest.class);
    request.setRoleId(UUID.randomUUID());
    var expectedPayload = createPayloadForRoleCapabilities();

    when(objectMapper.convertValue(any(), eq(ObjectNode.class))).thenReturn(expectedPayload);

    assertThrows(IllegalArgumentException.class, () -> sharingRoleCapabilityService.start(CONSORTIUM_ID, request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotEqualRoleIdPathId() {
    UUID roleId = UUID.fromString("999999-8367-4926-9999-514c35840399");

    var request = getMockDataObject(SHARING_ROLE_CAPABILITIES_REQUEST_SAMPLE, SharingRoleCapabilityRequest.class);

    assertThrows(java.lang.IllegalArgumentException.class,
      () -> sharingRoleCapabilityService.delete(CONSORTIUM_ID, roleId, request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotHavingPayloadOfRole() {
    var request = getMockDataObject(SHARING_ROLE_CAPABILITIES_WITHOUT_PAYLOAD_REQUEST_SAMPLE,
      SharingRoleCapabilityRequest.class);

    assertThrows(java.lang.IllegalArgumentException.class,
      () -> sharingRoleCapabilityService.delete(CONSORTIUM_ID, request.getRoleId(), request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotFound() {
    UUID roleId = UUID.fromString("4844767a-8367-4926-9999-514c35840399");

    var request = getMockDataObject(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE, SharingRoleCapabilityRequest.class);

    when(sharingRoleRepository.existsByRoleId(roleId)).thenReturn(false);

    assertThrows(ResourceNotFoundException.class,
      () -> sharingRoleCapabilityService.delete(CONSORTIUM_ID, roleId, request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Override
  protected Object getServiceUnderTest() {
    return sharingRoleCapabilityService;
  }
}
