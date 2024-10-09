package org.folio.consortia.service;

import com.fasterxml.jackson.databind.node.ObjectNode;

import org.folio.consortia.domain.dto.PublicationStatus;
import org.folio.consortia.domain.dto.SharingRoleCapabilitySetRequest;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.SharingRoleRepository;
import org.folio.consortia.service.impl.SharingRoleCapabilitySetService;
import org.folio.consortia.service.impl.SharingRoleService;
import org.folio.consortia.support.EntityUtils;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpMethod;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.folio.consortia.domain.dto.SourceValues.CONSORTIUM;
import static org.folio.consortia.domain.dto.SourceValues.USER;
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
  @Mock
  SharingRoleService sharingRoleService;

  @Test
  void shouldStartSharingRoleCapabilitySets() {
    var request = getMockDataObject(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE, SharingRoleCapabilitySetRequest.class);
    var roleIdForTenant1 = request.getRoleId();
    var roleIdForTenant2 = UUID.randomUUID();
    var createPcId = UUID.randomUUID();
    var updatePcId = UUID.randomUUID();
    var tenantSharedRoleAndCapabilitySets = Set.of(TENANT_ID_1);
    var sharingRoleEntity = createSharingRoleEntity(request.getRoleId(), TENANT_ID_2);
    var payloadForTenant1 = createPayloadForRoleCapabilitySets(roleIdForTenant1);
    var payloadForTenant2 = createPayloadForRoleCapabilitySets(roleIdForTenant2);
    var expectedPayloadTenant1 = createPayloadForRoleCapabilitySets(roleIdForTenant1);
    var expectedPayloadTenant2 = createPayloadForRoleCapabilitySets(roleIdForTenant2);

    // "tenant1" exists in tenant role association so that tenant1 is in PUT request publication,
    // "tenant2" is in POST method publication
    var expectedPubRequestPut = createPublicationRequest(CONSORTIUM.getRoleValue(), expectedPayloadTenant1, HttpMethod.PUT)
      .tenants(Set.of(TENANT_ID_1))
      .url("/roles/" + roleIdForTenant1 + "/capability-sets");
    var expectedPubRequestPost = createPublicationRequest(CONSORTIUM.getRoleValue(), expectedPayloadTenant2, HttpMethod.POST)
      .tenants(Set.of(TENANT_ID_2))
      .url(request.getUrl());
    var expectedSharingRoleEntity = createSharingRoleEntity(roleIdForTenant2, TENANT_ID_2);
    expectedSharingRoleEntity.setIsCapabilitySetsShared(true);

    setupCommonMocksForStart(createPcId, updatePcId, expectedPubRequestPost, expectedPubRequestPut, payloadForTenant1);
    when(objectMapper.convertValue(any(), eq(ObjectNode.class)))
      .thenReturn(payloadForTenant1)
      .thenReturn(payloadForTenant2);
    when(sharingRoleRepository.findTenantsByRoleNameAndIsCapabilitySetsSharedTrue(request.getRoleName()))
      .thenReturn(tenantSharedRoleAndCapabilitySets);
    when(sharingRoleRepository.findRoleIdByRoleNameAndTenantId(request.getRoleName(), TENANT_ID_1)).thenReturn(roleIdForTenant1);
    when(sharingRoleRepository.findRoleIdByRoleNameAndTenantId(request.getRoleName(), TENANT_ID_2)).thenReturn(roleIdForTenant2);
    when(sharingRoleRepository.findByRoleNameAndTenantId(request.getRoleName(), TENANT_ID_2))
      .thenReturn(Optional.of(sharingRoleEntity));
    when(sharingRoleRepository.saveAll(List.of(expectedSharingRoleEntity))).thenReturn(List.of(expectedSharingRoleEntity));

    var actualResponse = sharingRoleCapabilitySetService.start(CONSORTIUM_ID, request);

    assertThat(actualResponse.getCreatePCIds()).isEqualTo(List.of(createPcId));
    assertThat(actualResponse.getUpdatePCIds()).isEqualTo(List.of(updatePcId));

    verify(publicationService, times(2)).publishRequest(any(), any());
  }

  @Test
  void shouldDeleteSharingRoleCapabilitySets() {
    var pcId = UUID.randomUUID();
    var roleId = UUID.fromString("4844767a-8367-4926-9999-514c35840399");
    var tenantSharedRoleAndCapabilitySets = Set.of(TENANT_ID_1);
    var request = getMockDataObject(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE, SharingRoleCapabilitySetRequest.class);
    var payload = createPayloadForRoleCapabilitySets(roleId);

    var expectedPubRequestDelete = createPublicationRequest(HttpMethod.DELETE)
      .tenants(Set.of(TENANT_ID_1))
      .url("/roles/" + request.getRoleId() + "/capability-sets")
      .payload(payload);

    setupCommonMocksForDelete(pcId, expectedPubRequestDelete);
    when(objectMapper.convertValue(any(), eq(ObjectNode.class))).thenReturn(payload);
    when(sharingRoleRepository.findRoleIdByRoleNameAndTenantId(request.getRoleName(), TENANT_ID_1)).thenReturn(roleId);
    when(sharingRoleRepository.existsByRoleId(roleId)).thenReturn(true);
    when(sharingRoleRepository.findTenantsByRoleNameAndIsCapabilitySetsSharedTrue(request.getRoleName()))
      .thenReturn(tenantSharedRoleAndCapabilitySets);

    var actualResponse = sharingRoleCapabilitySetService.delete(CONSORTIUM_ID, roleId, request);

    assertThat(actualResponse.getPcIds()).isEqualTo(List.of(pcId));

    verify(publicationService, times(1)).publishRequest(any(), any());
  }

  @Test
  void shouldUpdateFailedTenantRoleCapabilitySets() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
    var request = getMockDataObject(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE, SharingRoleCapabilitySetRequest.class);
    var publicationId = UUID.randomUUID();
    var pcId1 = UUID.randomUUID();
    var pcId2 = UUID.randomUUID();
    var pubResultCollection = createPublicationResultCollection(CENTRAL_TENANT_ID, TENANT_ID_2);
    var pubDetails = createPublicationDetails(PublicationStatus.ERROR);
    var roleIdForTenant1 = request.getRoleId();
    var roleIdForTenant2 = UUID.randomUUID();
    var payloadForTenant1 = createPayloadForRoleCapabilitySets(roleIdForTenant1);
    var payloadForTenant2 = createPayloadForRoleCapabilitySets(roleIdForTenant2);
    var expectedPayloadTenant1 = createPayloadForRoleCapabilitySets(roleIdForTenant1);
    var expectedPayloadTenant2 = createPayloadForRoleCapabilitySets(roleIdForTenant2);

    var expectedPublicationRequest1 = createPublicationRequest(USER.getRoleValue(), expectedPayloadTenant1, HttpMethod.PUT)
      .url("/roles/" + roleIdForTenant1 + "/capability-sets")
      .tenants(Set.of(CENTRAL_TENANT_ID));
    var expectedPublicationRequest2 = createPublicationRequest(USER.getRoleValue(), expectedPayloadTenant2, HttpMethod.PUT)
      .url("/roles/" + roleIdForTenant2 + "/capability-sets")
      .tenants(Set.of(TENANT_ID_2));

    setupCommonMocksForDelete(pcId1, expectedPublicationRequest1);
    setupCommonMocksForDelete(pcId2, expectedPublicationRequest2);
    when(publicationService.checkPublicationDetailsExists(CONSORTIUM_ID, publicationId))
      .thenReturn(false)
      .thenReturn(false)
      .thenReturn(true);
    when(objectMapper.convertValue(request.getPayload(), ObjectNode.class))
      .thenReturn(payloadForTenant1)
      .thenReturn(payloadForTenant2);
    when(objectMapper.convertValue(payloadForTenant1, ObjectNode.class)).thenReturn(payloadForTenant1);
    when(objectMapper.convertValue(payloadForTenant2, ObjectNode.class)).thenReturn(payloadForTenant2);

    when(sharingRoleRepository.findRoleIdByRoleNameAndTenantId(request.getRoleName(), EntityUtils.CENTRAL_TENANT_ID)).thenReturn(roleIdForTenant1);
    when(sharingRoleRepository.findRoleIdByRoleNameAndTenantId(request.getRoleName(), TENANT_ID_2)).thenReturn(roleIdForTenant2);
    when(publicationService.getPublicationDetails(CONSORTIUM_ID, publicationId)).thenReturn(pubDetails);
    when(publicationService.getPublicationResults(CONSORTIUM_ID, publicationId)).thenReturn(pubResultCollection);

    Method method = SharingRoleCapabilitySetService.class.getSuperclass()
      .getDeclaredMethod("updateConfigsForFailedTenantsWithRetry", UUID.class, UUID.class, Object.class);
    method.setAccessible(true);
    method.invoke(sharingRoleCapabilitySetService, CONSORTIUM_ID, publicationId, request);

    verify(publicationService).getPublicationDetails(CONSORTIUM_ID, publicationId);
    verify(publicationService, times(3)).checkPublicationDetailsExists(CONSORTIUM_ID, publicationId);
    verify(publicationService).publishRequest(CONSORTIUM_ID, expectedPublicationRequest1);
    verify(publicationService).publishRequest(CONSORTIUM_ID, expectedPublicationRequest2);
  }

  @Test
  void shouldThrowErrorForNotEqualRoleIdWithPayloadId() {
    var request = getMockDataObject(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE, SharingRoleCapabilitySetRequest.class);
    request.setRoleId(UUID.randomUUID());
    var payload = createPayloadForRoleCapabilitySets(UUID.randomUUID());

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
