package org.folio.consortia.service;

import com.fasterxml.jackson.databind.node.ObjectNode;
import org.folio.consortia.domain.dto.PublicationStatus;
import org.folio.consortia.domain.dto.SharingSettingRequest;
import org.folio.consortia.domain.entity.SharingSettingEntity;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.SharingSettingRepository;
import org.folio.consortia.service.impl.SharingSettingService;
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
import static org.folio.consortia.support.EntityUtils.SHARING_SETTING_REQUEST_SAMPLE_FOR_DEPARTMENT;
import static org.folio.consortia.support.EntityUtils.SHARING_SETTING_REQUEST_SAMPLE_FOR_GROUP;
import static org.folio.consortia.support.EntityUtils.SHARING_SETTING_REQUEST_SAMPLE_WITHOUT_PAYLOAD;
import static org.folio.consortia.support.EntityUtils.TENANT_ID_1;
import static org.folio.consortia.support.EntityUtils.TENANT_ID_2;
import static org.folio.consortia.support.EntityUtils.createPayloadForDepartment;
import static org.folio.consortia.support.EntityUtils.createPayloadForGroup;
import static org.folio.consortia.support.EntityUtils.createPublicationDetails;
import static org.folio.consortia.support.EntityUtils.createPublicationRequest;
import static org.folio.consortia.support.EntityUtils.createPublicationResultCollection;
import static org.folio.consortia.support.EntityUtils.createSharingSettingEntity;
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
class SharingSettingServiceTest extends BaseSharingConfigServiceTest {

  @InjectMocks
  SharingSettingService sharingSettingService;
  @Mock
  SharingSettingRepository sharingSettingRepository;

  @Test
  void shouldStartSharingSetting() {
    var createPcId = UUID.randomUUID();
    var updatePcId = UUID.randomUUID();
    var tenantsSharedSetting = Set.of(TENANT_ID_1);
    var request = getMockDataObject(SHARING_SETTING_REQUEST_SAMPLE_FOR_DEPARTMENT, SharingSettingRequest.class);
    var payload = createPayloadForDepartment();

    // "tenant1" exists in tenant setting association so that tenant1 is in PUT request publication,
    // "tenant2" is in POST method publication
    var expectedPubRequestPOST = createPublicationRequest(CONSORTIUM.getSettingValue(), payload, HttpMethod.POST)
      .tenants(Set.of(TENANT_ID_2))
      .url(request.getUrl());
    var expectedPubRequestPUT = createPublicationRequest(CONSORTIUM.getSettingValue(), payload, HttpMethod.PUT)
      .tenants(Set.of(TENANT_ID_1))
      .url(request.getUrl() + "/" + request.getSettingId());
    var expectedSharingSettingEntity = createSharingSettingEntity(request.getSettingId(), TENANT_ID_2);

    setupCommonMocksForStart(createPcId, updatePcId, expectedPubRequestPOST, expectedPubRequestPUT, payload);
    when(sharingSettingRepository.findTenantsBySettingId(request.getSettingId())).thenReturn(tenantsSharedSetting);
    when(sharingSettingRepository.save(expectedSharingSettingEntity)).thenReturn(new SharingSettingEntity());

    var actualResponse = sharingSettingService.start(CONSORTIUM_ID, request);

    assertThat(actualResponse.getCreateSettingsPCId()).isEqualTo(createPcId);
    assertThat(actualResponse.getUpdateSettingsPCId()).isEqualTo(updatePcId);

    verify(publicationService, times(2)).publishRequest(any(), any());
  }

  @Test
  void shouldDeleteSharingSetting() {
    var pcId = UUID.randomUUID();
    var settingId = UUID.fromString("1844767a-8367-4926-9999-514c35840399");
    var tenantsSharedSetting = Set.of(TENANT_ID_1);
    var request = getMockDataObject(SHARING_SETTING_REQUEST_SAMPLE_FOR_DEPARTMENT, SharingSettingRequest.class);

    // "tenant1" exists in tenant setting association so that tenant1 is in DELETE request publication,
    var expectedPubRequestDELETE = createPublicationRequest(HttpMethod.DELETE)
      .tenants(Set.of(TENANT_ID_1))
      .url(request.getUrl() + "/" + request.getSettingId())
      .payload(request.getPayload());

    setupCommonMocksForDelete(pcId, expectedPubRequestDELETE);
    when(sharingSettingRepository.existsBySettingId(settingId)).thenReturn(true);
    when(sharingSettingRepository.findTenantsBySettingId(request.getSettingId())).thenReturn(tenantsSharedSetting);

    var actualResponse = sharingSettingService.delete(CONSORTIUM_ID, settingId, request);

    assertThat(actualResponse.getPcId()).isEqualTo(pcId);

    verify(publicationService, times(1)).publishRequest(any(), any());
  }

  @Test
  void shouldUpdateFailedTenantSettings() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
    var publicationId = UUID.randomUUID();
    var pcId = UUID.randomUUID();
    var request = getMockDataObject(SHARING_SETTING_REQUEST_SAMPLE_FOR_GROUP, SharingSettingRequest.class);
    var pubResultCollection = createPublicationResultCollection(CENTRAL_TENANT_ID, TENANT_ID_2);
    var pubDetails = createPublicationDetails(PublicationStatus.ERROR);
    var payload = createPayloadForGroup();

    // expected data for publish request
    var expectedPubRequest = createPublicationRequest(CONSORTIUM.getSettingValue(), payload, HttpMethod.PUT)
      .tenants(Set.of(CENTRAL_TENANT_ID, TENANT_ID_2))
      .url(request.getUrl() + "/" + request.getSettingId());

    setupCommonMocksForDelete(pcId, expectedPubRequest);
    when(publicationService.checkPublicationDetailsExists(CONSORTIUM_ID, publicationId))
      .thenReturn(false)
      .thenReturn(false)
      .thenReturn(true);
    when(publicationService.getPublicationDetails(CONSORTIUM_ID, publicationId)).thenReturn(pubDetails);
    when(publicationService.getPublicationResults(CONSORTIUM_ID, publicationId)).thenReturn(pubResultCollection);
    when(objectMapper.convertValue(any(), eq(ObjectNode.class))).thenReturn(payload);

    // Use reflection to access the protected method in BaseSharingService
    Method method = SharingSettingService.class.getSuperclass()
      .getDeclaredMethod("updateConfigsForFailedTenantsWithRetry", UUID.class, UUID.class, Object.class);
    method.setAccessible(true);
    method.invoke(sharingSettingService, CONSORTIUM_ID, publicationId, request);

    verify(publicationService).getPublicationDetails(CONSORTIUM_ID, publicationId);
    verify(publicationService, times(3))
      .checkPublicationDetailsExists(CONSORTIUM_ID, publicationId);
    verify(publicationService).publishRequest(CONSORTIUM_ID, expectedPubRequest);
  }

  // Negative cases
  @Test
  void shouldThrowErrorForNotEqualSettingIdWithPayloadId() {
    var request = getMockDataObject(SHARING_SETTING_REQUEST_SAMPLE_FOR_DEPARTMENT, SharingSettingRequest.class);
    request.setSettingId(UUID.randomUUID());
    var node = createPayloadForDepartment();

    when(objectMapper.convertValue(any(), eq(ObjectNode.class))).thenReturn(node);

    assertThrows(java.lang.IllegalArgumentException.class,
      () -> sharingSettingService.start(CONSORTIUM_ID, request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotEqualSettingIdPathId() {
    var settingId = UUID.fromString("999999-8367-4926-9999-514c35840399");
    var request = getMockDataObject(SHARING_SETTING_REQUEST_SAMPLE_FOR_GROUP, SharingSettingRequest.class);

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);

    assertThrows(java.lang.IllegalArgumentException.class,
      () -> sharingSettingService.delete(CONSORTIUM_ID, settingId, request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotHavingPayloadOfSetting() {
    var request = getMockDataObject(SHARING_SETTING_REQUEST_SAMPLE_WITHOUT_PAYLOAD, SharingSettingRequest.class);

    assertThrows(java.lang.IllegalArgumentException.class,
      () -> sharingSettingService.delete(CONSORTIUM_ID, request.getSettingId(), request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Test
  void shouldThrowErrorForNotFound() {
    var settingId = UUID.fromString("1844767a-8367-4926-9999-514c35840399");
    var request = getMockDataObject(SHARING_SETTING_REQUEST_SAMPLE_FOR_DEPARTMENT, SharingSettingRequest.class);

    when(sharingSettingRepository.existsBySettingId(settingId)).thenReturn(false);

    assertThrows(ResourceNotFoundException.class,
      () -> sharingSettingService.delete(CONSORTIUM_ID, settingId, request));
    verify(publicationService, times(0)).publishRequest(any(), any());
  }

  @Override
  protected Object getServiceUnderTest() {
    return sharingSettingService;
  }
}
