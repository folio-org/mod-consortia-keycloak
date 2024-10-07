package org.folio.consortia.service;

import com.fasterxml.jackson.databind.node.ObjectNode;

import feign.FeignException;
import org.folio.consortia.client.RolesClient;
import org.folio.consortia.domain.dto.PublicationStatus;
import org.folio.consortia.domain.dto.SharingRoleRequest;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.SharingRoleRepository;
import org.folio.consortia.service.impl.SharingRoleService;
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
import static org.folio.consortia.domain.dto.SourceValues.USER;
import static org.folio.consortia.support.EntityUtils.CENTRAL_TENANT_ID;
import static org.folio.consortia.support.EntityUtils.SHARING_ROLE_REQUEST_SAMPLE;
import static org.folio.consortia.support.EntityUtils.SHARING_ROLE_REQUEST_SAMPLE_WITHOUT_PAYLOAD;
import static org.folio.consortia.support.EntityUtils.TENANT_ID_1;
import static org.folio.consortia.support.EntityUtils.TENANT_ID_2;
import static org.folio.consortia.support.EntityUtils.createPayloadForRole;
import static org.folio.consortia.support.EntityUtils.createPublicationDetails;
import static org.folio.consortia.support.EntityUtils.createPublicationRequest;
import static org.folio.consortia.support.EntityUtils.createPublicationResultCollection;
import static org.folio.consortia.support.EntityUtils.createSharingRoleEntity;
import static org.folio.consortia.support.TestConstants.CONSORTIUM_ID;
import static org.folio.consortia.utils.InputOutputTestUtils.getMockDataObject;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SpringBootTest
class SharingRoleServiceTest extends BaseSharingConfigServiceTest {

  @InjectMocks
  SharingRoleService sharingRoleService;
  @Mock
  SharingRoleRepository sharingRoleRepository;
  @Mock
  RolesClient rolesClient;

  @Test
  void shouldStartSharingRole() {
    var request = getMockDataObject(SHARING_ROLE_REQUEST_SAMPLE, SharingRoleRequest.class);
    var roleIdForTenant1 = request.getRoleId();
    var roleIdForTenant2 = UUID.randomUUID();
    var createPcId = UUID.randomUUID();
    var updatePcId = UUID.randomUUID();
    var tenantsSharedRole = Set.of(TENANT_ID_1);

    var payloadForTenant1 = createPayloadForRole(roleIdForTenant1.toString(), request.getRoleName());
    var payloadForTenant2 = createPayloadForRole(roleIdForTenant2.toString(), request.getRoleName());

    // "tenant1" exists in tenant role association so that tenant1 is in PUT request publication,
    // "tenant2" is in POST method publication
    var expectedPuRequestPUT = createPublicationRequest(CONSORTIUM.getRoleValue(), payloadForTenant1, HttpMethod.PUT)
      .tenants(Set.of(TENANT_ID_1))
      .url(request.getUrl() + "/" + roleIdForTenant1);
    var expectedPubRequestPOST = createPublicationRequest(CONSORTIUM.getRoleValue(), payloadForTenant2, HttpMethod.POST)
      .tenants(Set.of(TENANT_ID_2))
      .url(request.getUrl());
    var expectedSharingRoleEntity = createSharingRoleEntity(roleIdForTenant2, TENANT_ID_2);

    setupCommonMocksForStart(createPcId, updatePcId, expectedPubRequestPOST, expectedPuRequestPUT, payloadForTenant1);
    when(objectMapper.convertValue(any(), eq(ObjectNode.class)))
      .thenReturn(payloadForTenant1)
      .thenReturn(payloadForTenant2);
    when(rolesClient.getRolesByQuery(any()))
      .thenThrow(new FeignException.NotFound("Role not found", buildFeignRequest(), null, null));
    when(sharingRoleRepository.findRoleIdByRoleNameAndTenantId(request.getRoleName(), TENANT_ID_1)).thenReturn(roleIdForTenant1);
    when(sharingRoleRepository.findTenantsByRoleName(request.getRoleName())).thenReturn(tenantsSharedRole);
    when(sharingRoleRepository.save(expectedSharingRoleEntity)).thenReturn(expectedSharingRoleEntity);

    var actualResponse = sharingRoleService.start(CONSORTIUM_ID, request);

    assertThat(actualResponse.getCreatePCIds()).isEqualTo(List.of(createPcId));
    assertThat(actualResponse.getUpdatePCIds()).isEqualTo(List.of(updatePcId));

    verify(publicationService, times(2)).publishRequest(any(), any());
    verify(rolesClient).getRolesByQuery(any());
  }

  @Test
  void shouldDeleteSharingRole() {
    var pcId = UUID.randomUUID();
    var roleId = UUID.fromString("3844767a-8367-4926-9999-514c35840399");
    var tenantsSharedRole = Set.of(TENANT_ID_1);
    var request = getMockDataObject(SHARING_ROLE_REQUEST_SAMPLE, SharingRoleRequest.class);
    var payload = createPayloadForRole(roleId.toString(), request.getRoleName());

    // "tenant1" exists in the tenant role association so that tenant1 is in DELETE request publication,
    var expectedPubRequestDELETE = createPublicationRequest(HttpMethod.DELETE)
      .tenants(Set.of(TENANT_ID_1))
      .url(request.getUrl() + "/" + roleId)
      .payload(payload);

    setupCommonMocksForDelete(pcId, expectedPubRequestDELETE);
    when(objectMapper.convertValue(any(), eq(ObjectNode.class))).thenReturn(payload);
    when(rolesClient.getRolesByQuery(any()))
      .thenThrow(new FeignException.NotFound("Role not found", buildFeignRequest(), null, null));
    when(sharingRoleRepository.findRoleIdByRoleNameAndTenantId(request.getRoleName(), TENANT_ID_1)).thenReturn(request.getRoleId());
    when(sharingRoleRepository.existsByRoleId(roleId)).thenReturn(true);
    when(sharingRoleRepository.findTenantsByRoleName(request.getRoleName())).thenReturn(tenantsSharedRole);

    var actualResponse = sharingRoleService.delete(CONSORTIUM_ID, roleId, request);

    assertThat(actualResponse.getPcIds()).isEqualTo(List.of(pcId));

    verify(publicationService, times(1)).publishRequest(any(), any());
    verify(rolesClient).getRolesByQuery(any());
  }

  @Test
  void shouldUpdateFailedTenantRole() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
    var request = getMockDataObject(SHARING_ROLE_REQUEST_SAMPLE, SharingRoleRequest.class);
    var publicationId = UUID.randomUUID();
    var pcId1 = UUID.randomUUID();
    var pcId2 = UUID.randomUUID();
    var pubResultCollection = createPublicationResultCollection(CENTRAL_TENANT_ID, TENANT_ID_2);
    var pubDetails = createPublicationDetails(PublicationStatus.ERROR);
    var roleIdForTenant1 = request.getRoleId();
    var roleIdForTenant2 = UUID.randomUUID();
    var payloadForTenant1 = createPayloadForRole(roleIdForTenant1.toString(), request.getRoleName());
    var payloadForTenant2 = createPayloadForRole(roleIdForTenant2.toString(), request.getRoleName());
    var expectedPayloadTenant1 = createPayloadForRole(roleIdForTenant1.toString(), request.getRoleName());
    var expectedPayloadTenant2 = createPayloadForRole(roleIdForTenant2.toString(), request.getRoleName());

    // expected data for publish request
    var expectedPubRequest1 = createPublicationRequest(USER.getRoleValue(), expectedPayloadTenant1, HttpMethod.PUT)
      .tenants(Set.of(CENTRAL_TENANT_ID))
      .url(request.getUrl() + "/" + roleIdForTenant1);
    var expectedPubRequest2 = createPublicationRequest(USER.getRoleValue(), expectedPayloadTenant2, HttpMethod.PUT)
      .url(request.getUrl() + "/" + roleIdForTenant2)
      .tenants(Set.of(TENANT_ID_2));

    setupCommonMocksForDelete(pcId1, expectedPubRequest1);
    setupCommonMocksForDelete(pcId2, expectedPubRequest2);
    when(publicationService.checkPublicationDetailsExists(CONSORTIUM_ID, publicationId))
      .thenReturn(false)
      .thenReturn(false)
      .thenReturn(true);
    when(objectMapper.convertValue(eq(request.getPayload()), eq(ObjectNode.class)))
      .thenReturn(payloadForTenant1)
      .thenReturn(payloadForTenant2);
    when(objectMapper.convertValue(eq(payloadForTenant1), eq(ObjectNode.class))).thenReturn(payloadForTenant1);
    when(objectMapper.convertValue(eq(payloadForTenant2), eq(ObjectNode.class))).thenReturn(payloadForTenant2);

    when(sharingRoleRepository.findRoleIdByRoleNameAndTenantId(request.getRoleName(), CENTRAL_TENANT_ID)).thenReturn(roleIdForTenant1);
    when(sharingRoleRepository.findRoleIdByRoleNameAndTenantId(request.getRoleName(), TENANT_ID_2)).thenReturn(roleIdForTenant2);
    when(publicationService.getPublicationDetails(CONSORTIUM_ID, publicationId)).thenReturn(pubDetails);
    when(publicationService.getPublicationResults(CONSORTIUM_ID, publicationId)).thenReturn(pubResultCollection);

    // Use reflection to access the protected method in BaseSharingService
    Method method = SharingRoleService.class.getSuperclass()
      .getDeclaredMethod("updateConfigsForFailedTenantsWithRetry", UUID.class, UUID.class, Object.class);
    method.setAccessible(true);
    method.invoke(sharingRoleService, CONSORTIUM_ID, publicationId, request);

    verify(publicationService).getPublicationDetails(CONSORTIUM_ID, publicationId);
    verify(publicationService, times(3))
      .checkPublicationDetailsExists(CONSORTIUM_ID, publicationId);
    verify(publicationService).publishRequest(CONSORTIUM_ID, expectedPubRequest1);
    verify(publicationService).publishRequest(CONSORTIUM_ID, expectedPubRequest2);
  }

  // Negative cases
  @Test
  void shouldThrowErrorForNotEqualRoleIdWithPayloadId() {
    var request = getMockDataObject(SHARING_ROLE_REQUEST_SAMPLE, SharingRoleRequest.class);
    request.setRoleId(UUID.randomUUID());
    var payload = createPayloadForRole(UUID.randomUUID().toString(), request.getRoleName());

    when(objectMapper.convertValue(any(), eq(ObjectNode.class))).thenReturn(payload);

    assertThrows(java.lang.IllegalArgumentException.class,
      () -> sharingRoleService.start(CONSORTIUM_ID, request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotEqualRoleIdPathId() {
    var roleId = UUID.fromString("999999-8367-4926-9999-514c35840399");
    var request = getMockDataObject(SHARING_ROLE_REQUEST_SAMPLE, SharingRoleRequest.class);

    assertThrows(java.lang.IllegalArgumentException.class,
      () -> sharingRoleService.delete(CONSORTIUM_ID, roleId, request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotHavingPayloadOfRole() {
    var request = getMockDataObject(SHARING_ROLE_REQUEST_SAMPLE_WITHOUT_PAYLOAD, SharingRoleRequest.class);

    assertThrows(IllegalArgumentException.class,
      () -> sharingRoleService.delete(CONSORTIUM_ID, request.getRoleId(), request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotFound() {
    UUID roleId = UUID.fromString("3844767a-8367-4926-9999-514c35840399");
    var request = getMockDataObject(SHARING_ROLE_REQUEST_SAMPLE, SharingRoleRequest.class);

    when(sharingRoleRepository.existsByRoleId(roleId)).thenReturn(false);

    assertThrows(ResourceNotFoundException.class,
      () -> sharingRoleService.delete(CONSORTIUM_ID, roleId, request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Override
  protected Object getServiceUnderTest() {
    return sharingRoleService;
  }
}
