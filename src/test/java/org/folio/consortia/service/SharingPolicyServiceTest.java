package org.folio.consortia.service;

import com.fasterxml.jackson.databind.node.ObjectNode;
import org.folio.consortia.domain.dto.PublicationStatus;
import org.folio.consortia.domain.dto.SharingPolicyRequest;
import org.folio.consortia.domain.entity.SharingPolicyEntity;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.SharingPolicyRepository;
import org.folio.consortia.service.impl.SharingPolicyService;
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
import static org.folio.consortia.domain.dto.SourceValues.CONSORTIUM;
import static org.folio.consortia.support.EntityUtils.CENTRAL_TENANT_ID;
import static org.folio.consortia.support.EntityUtils.SHARING_POLICY_REQUEST_SAMPLE_FOR_ROLES;
import static org.folio.consortia.support.EntityUtils.SHARING_POLICY_REQUEST_SAMPLE_WITHOUT_PAYLOAD;
import static org.folio.consortia.support.EntityUtils.TENANT_ID_1;
import static org.folio.consortia.support.EntityUtils.TENANT_ID_2;
import static org.folio.consortia.support.EntityUtils.createPayloadForPolicy;
import static org.folio.consortia.support.EntityUtils.createPublicationDetails;
import static org.folio.consortia.support.EntityUtils.createPublicationRequest;
import static org.folio.consortia.support.EntityUtils.createPublicationResultCollection;
import static org.folio.consortia.support.EntityUtils.createSharingPolicyEntity;
import static org.folio.consortia.support.TestConstants.CONSORTIUM_ID;
import static org.folio.consortia.utils.InputOutputTestUtils.getMockDataObject;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SpringBootTest
class SharingPolicyServiceTest extends BaseSharingConfigServiceTest{

  @InjectMocks
  SharingPolicyService sharingPolicyService;
  @Mock
  SharingPolicyRepository sharingPolicyRepository;

  @Test
  void shouldStartSharingPolicy() {
    var createPcId = UUID.randomUUID();
    var updatePcId = UUID.randomUUID();
    var tenantSharedPolicy = Set.of(TENANT_ID_1);
    var request = getMockDataObject(SHARING_POLICY_REQUEST_SAMPLE_FOR_ROLES, SharingPolicyRequest.class);
    var payload = createPayloadForPolicy();

    // "tenant1" exists in tenant policy association so that tenant1 is in PUT request publication,
    // "tenant2" is in POST method publication
    var expectedPubRequestPost = createPublicationRequest(CONSORTIUM.getPolicyValue(), payload, HttpMethod.POST)
      .tenants(Set.of(TENANT_ID_2))
      .url(request.getUrl());
    var expectedPubRequestPut = createPublicationRequest(CONSORTIUM.getPolicyValue(), payload, HttpMethod.PUT)
      .tenants(Set.of(TENANT_ID_1))
      .url(request.getUrl() + "/" + request.getPolicyId());
    var expectedSharingPolicyEntity = createSharingPolicyEntity(request.getPolicyId(), TENANT_ID_2);

    setupCommonMocksForStart(createPcId, updatePcId, expectedPubRequestPost, expectedPubRequestPut, payload);
    when(sharingPolicyRepository.findTenantsByPolicyId(request.getPolicyId())).thenReturn(tenantSharedPolicy);
    when(sharingPolicyRepository.save(expectedSharingPolicyEntity)).thenReturn(new SharingPolicyEntity());

    var actualResponse = sharingPolicyService.start(CONSORTIUM_ID, request);

    assertThat(actualResponse.getCreatePoliciesPCId()).isEqualTo(createPcId);
    assertThat(actualResponse.getUpdatePoliciesPCId()).isEqualTo(updatePcId);

    verify(publicationService, times(2)).publishRequest(any(), any());
  }

  @Test
  void shouldDeleteSharingPolicy() {
    var pcId = UUID.randomUUID();
    var policyId = UUID.fromString("2844767a-8367-4926-9999-514c35840399");
    var tenantsSharedPolicy = Set.of(TENANT_ID_1);
    var request = getMockDataObject(SHARING_POLICY_REQUEST_SAMPLE_FOR_ROLES, SharingPolicyRequest.class);

    // "tenant1" exists in tenant policy association so that tenant1 is in DELETE request publication,
    var expectedPubRequestDelete = createPublicationRequest(HttpMethod.DELETE)
      .tenants(Set.of(TENANT_ID_1))
      .url(request.getUrl() + "/" + policyId)
      .payload(request.getPayload());

    setupCommonMocksForDelete(pcId, expectedPubRequestDelete);
    when(sharingPolicyRepository.existsByPolicyId(policyId)).thenReturn(true);
    when(sharingPolicyRepository.findTenantsByPolicyId(request.getPolicyId())).thenReturn(tenantsSharedPolicy);

    var actualResponse = sharingPolicyService.delete(CONSORTIUM_ID, policyId, request);

    assertThat(actualResponse.getPcId()).isEqualTo(pcId);

    verify(publicationService, times(1)).publishRequest(any(), any());
  }

  @Test
  void shouldUpdateFailedTenantPolicies() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
    var publicationId = UUID.randomUUID();
    var pcId = UUID.randomUUID();
    var request = getMockDataObject(SHARING_POLICY_REQUEST_SAMPLE_FOR_ROLES, SharingPolicyRequest.class);
    var pubResultCollection = createPublicationResultCollection(CENTRAL_TENANT_ID, TENANT_ID_2);
    var pubDetails = createPublicationDetails(PublicationStatus.ERROR);
    var payload = createPayloadForPolicy();

    // expected data for publish request
    var expectedPubRequest = createPublicationRequest(CONSORTIUM.getPolicyValue(), payload, HttpMethod.PUT)
      .tenants(Set.of(CENTRAL_TENANT_ID, TENANT_ID_2))
      .url(request.getUrl() + "/" + request.getPolicyId());

    setupCommonMocksForDelete(pcId, expectedPubRequest);
    when(publicationService.checkPublicationDetailsExists(CONSORTIUM_ID, publicationId))
      .thenReturn(false)
      .thenReturn(false)
      .thenReturn(true);
    when(publicationService.getPublicationDetails(CONSORTIUM_ID, publicationId)).thenReturn(pubDetails);
    when(publicationService.getPublicationResults(CONSORTIUM_ID, publicationId)).thenReturn(pubResultCollection);
    when(objectMapper.convertValue(any(), eq(ObjectNode.class))).thenReturn(payload);

    // Use reflection to access the protected method in BaseSharingService
    Method method = SharingPolicyService.class.getSuperclass()
      .getDeclaredMethod("updateConfigsForFailedTenantsWithRetry", UUID.class, UUID.class, Object.class);
    method.setAccessible(true);
    method.invoke(sharingPolicyService, CONSORTIUM_ID, publicationId, request);

    verify(publicationService).getPublicationDetails(CONSORTIUM_ID, publicationId);
    verify(publicationService, times(3))
      .checkPublicationDetailsExists(CONSORTIUM_ID, publicationId);
    verify(publicationService).publishRequest(CONSORTIUM_ID, expectedPubRequest);
  }

  // Negative cases
  @Test
  void shouldThrowErrorForNotEqualPolicyIdWithPayloadId() {
    var request = getMockDataObject(SHARING_POLICY_REQUEST_SAMPLE_FOR_ROLES, SharingPolicyRequest.class);
    request.setPolicyId(UUID.randomUUID());
    var payload = createPayloadForPolicy();

    when(objectMapper.convertValue(any(), eq(ObjectNode.class))).thenReturn(payload);

    assertThrows(java.lang.IllegalArgumentException.class, () -> sharingPolicyService.start(CONSORTIUM_ID, request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotEqualPolicyIdPathId() {
    UUID policyId = UUID.fromString("999999-8367-4926-9999-514c35840399");
    var request = getMockDataObject(SHARING_POLICY_REQUEST_SAMPLE_FOR_ROLES, SharingPolicyRequest.class);

    assertThrows(java.lang.IllegalArgumentException.class,
      () -> sharingPolicyService.delete(CONSORTIUM_ID, policyId, request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotHavingPayloadOfPolicy() {
    var sharingPolicyRequest = getMockDataObject(SHARING_POLICY_REQUEST_SAMPLE_WITHOUT_PAYLOAD, SharingPolicyRequest.class);

    assertThrows(java.lang.IllegalArgumentException.class,
      () -> sharingPolicyService.delete(CONSORTIUM_ID, sharingPolicyRequest.getPolicyId(), sharingPolicyRequest));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotFound() {
    var policyId = UUID.fromString("2844767a-8367-4926-9999-514c35840399");
    var request = getMockDataObject(SHARING_POLICY_REQUEST_SAMPLE_FOR_ROLES, SharingPolicyRequest.class);

    when(sharingPolicyRepository.existsByPolicyId(policyId)).thenReturn(false);

    assertThrows(ResourceNotFoundException.class,
      () -> sharingPolicyService.delete(CONSORTIUM_ID, policyId, request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Override
  protected Object getServiceUnderTest() {
    return sharingPolicyService;
  }
}
