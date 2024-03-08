package org.folio.consortia.service.impl;

import static org.folio.consortia.utils.InputOutputTestUtils.getMockDataObject;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Set;
import org.folio.consortia.domain.dto.PublicationHttpResponse;
import org.folio.consortia.domain.entity.PublicationStatusEntity;
import org.folio.consortia.domain.entity.PublicationTenantRequestEntity;
import org.folio.consortia.exception.PublicationException;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.PublicationStatusRepository;
import org.folio.consortia.repository.PublicationTenantRequestRepository;
import org.folio.consortia.service.ConsortiumService;
import org.folio.consortia.service.HttpRequestService;
import java.time.LocalDateTime;
import java.util.UUID;
import java.util.concurrent.CompletionException;

import org.folio.consortia.domain.dto.PublicationRequest;
import org.folio.consortia.domain.dto.PublicationStatus;
import org.folio.consortia.service.TenantService;
import org.folio.consortia.service.UserTenantService;
import org.folio.consortia.support.BaseUnitTest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.client.HttpClientErrorException;
import org.testcontainers.shaded.org.apache.commons.lang3.RandomStringUtils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

class PublicationServiceImplTest extends BaseUnitTest {
  private static final String PUBLICATION_REQUEST_SAMPLE = "mockdata/publications/publication_request.json";
  private static final String PUBLICATION_STATUS_ENTITY_SAMPLE = "mockdata/publications/publication_status_entity.json";
  @InjectMocks
  private PublicationServiceImpl publicationService;
  @Mock
  PublicationTenantRequestRepository publicationTenantRequestRepository;
  @Mock
  PublicationStatusRepository publicationStatusRepository;
  @Mock
  HttpRequestService httpRequestService;
  @Mock
  ObjectMapper objectMapper;
  @Mock
  ConsortiumService consortiumService;
  @Captor
  ArgumentCaptor<PublicationTenantRequestEntity> ptreCaptor;
  @Mock
  private UserTenantService userTenantService;
  @Mock
  private TenantService tenantService;

  @Test
  void createTenantRequestEntitiesSuccess() throws JsonProcessingException {
    ReflectionTestUtils.setField(publicationService, "maxActiveThreads", 5);

    PublicationRequest pr = getMockDataObject(PUBLICATION_REQUEST_SAMPLE, PublicationRequest.class);
    var publicationStatusEntity = getMockDataObject(PUBLICATION_STATUS_ENTITY_SAMPLE, PublicationStatusEntity.class);
    publicationStatusEntity.setCreatedDate(LocalDateTime.now());

    when(objectMapper.writeValueAsString(anyString())).thenReturn(RandomStringUtils.random(10));
    when(publicationTenantRequestRepository.save(any(PublicationTenantRequestEntity.class))).thenReturn(new PublicationTenantRequestEntity());

    publicationService.processTenantRequests(pr, publicationStatusEntity);

    verify(publicationTenantRequestRepository, atLeast(pr.getTenants().size())).save(any());
  }

  @Test
  void executeAsyncHttpRequestSuccess() throws JsonProcessingException {
    var pr = getMockDataObject(PUBLICATION_REQUEST_SAMPLE, PublicationRequest.class);
    var payload = objectMapper.writeValueAsString(pr.getPayload());
    var publicationStatusEntity = getMockDataObject(PUBLICATION_STATUS_ENTITY_SAMPLE, PublicationStatusEntity.class);
    publicationStatusEntity.setCreatedDate(LocalDateTime.now());

    when(objectMapper.writeValueAsString(anyString())).thenReturn(RandomStringUtils.random(10));
    when(publicationTenantRequestRepository.save(any(PublicationTenantRequestEntity.class))).thenReturn(new PublicationTenantRequestEntity());

    var restTemplateResponse = new PublicationHttpResponse(payload, HttpStatusCode.valueOf(201));
    when(httpRequestService.performRequest(anyString(), eq(HttpMethod.POST), any())).thenReturn(restTemplateResponse);
    var response = publicationService.executeHttpRequest(pr, CENTRAL_TENANT_NAME, folioExecutionContext);
    Assertions.assertEquals(payload, response.getBody());
  }
  @Test
  void executeAsyncHttpWithErrorResponse() throws JsonProcessingException {
    var pr = getMockDataObject(PUBLICATION_REQUEST_SAMPLE, PublicationRequest.class);
    var payload = objectMapper.writeValueAsString(pr.getPayload());
    var publicationStatusEntity = getMockDataObject(PUBLICATION_STATUS_ENTITY_SAMPLE, PublicationStatusEntity.class);
    publicationStatusEntity.setCreatedDate(LocalDateTime.now());

    var restTemplateResponse = new PublicationHttpResponse(payload, HttpStatusCode.valueOf(301));
    when(httpRequestService.performRequest(anyString(), eq(HttpMethod.POST), any())).thenReturn(restTemplateResponse);

    assertThrows(HttpClientErrorException.class, () -> publicationService.executeHttpRequest(pr, CENTRAL_TENANT_NAME, folioExecutionContext));
  }

  @Test
  void executeAsyncHttpFailure() {
    var pr = getMockDataObject(PUBLICATION_REQUEST_SAMPLE, PublicationRequest.class);
    var publicationStatusEntity = getMockDataObject(PUBLICATION_STATUS_ENTITY_SAMPLE, PublicationStatusEntity.class);
    publicationStatusEntity.setCreatedDate(LocalDateTime.now());

    doThrow(new HttpClientErrorException(HttpStatus.BAD_REQUEST, HttpStatus.BAD_REQUEST.getReasonPhrase()))
      .when(httpRequestService).performRequest(anyString(), eq(HttpMethod.POST), any());

    assertThrows(HttpClientErrorException.class, () -> publicationService.executeHttpRequest(pr, CENTRAL_TENANT_NAME, folioExecutionContext));
  }


  @Test
  void updatePublicationTenantRequestOnSuccess() {
    PublicationTenantRequestEntity ptrEntity = new PublicationTenantRequestEntity();
    var pse = getMockDataObject(PUBLICATION_STATUS_ENTITY_SAMPLE, PublicationStatusEntity.class);
    ptrEntity.setPcState(pse);
    ptrEntity.setStatus(PublicationStatus.IN_PROGRESS);
    when(publicationTenantRequestRepository.save(any(PublicationTenantRequestEntity.class))).thenReturn(new PublicationTenantRequestEntity());
    when(publicationStatusRepository.save(any(PublicationStatusEntity.class))).thenReturn(new PublicationStatusEntity());

    var payload = RandomStringUtils.random(10);
    var restTemplateResponse = new PublicationHttpResponse(payload, HttpStatusCode.valueOf(201));

    publicationService.updateSucceedPublicationTenantRequest(restTemplateResponse, ptrEntity);
    verify(publicationTenantRequestRepository).save(ptreCaptor.capture());

    var capturedPtre = ptreCaptor.getValue();
    Assertions.assertEquals(PublicationStatus.COMPLETE, capturedPtre.getStatus());
    Assertions.assertEquals(payload, capturedPtre.getResponse());
    Assertions.assertEquals(HttpStatus.CREATED.value(), capturedPtre.getResponseStatusCode());
  }

  @Test
  void updatePublicationTenantRequestOnFailure() {
    PublicationTenantRequestEntity ptrEntity = new PublicationTenantRequestEntity();
    var pse = getMockDataObject(PUBLICATION_STATUS_ENTITY_SAMPLE, PublicationStatusEntity.class);
    ptrEntity.setPcState(pse);
    ptrEntity.setStatus(PublicationStatus.IN_PROGRESS);
    when(publicationTenantRequestRepository.save(any(PublicationTenantRequestEntity.class))).thenReturn(new PublicationTenantRequestEntity());

    Throwable t = new CompletionException(new HttpClientErrorException(HttpStatusCode.valueOf(400), HttpStatus.BAD_REQUEST.getReasonPhrase()));
    publicationService.updateFailedPublicationTenantRequest(t, ptrEntity);
    verify(publicationTenantRequestRepository).save(ptreCaptor.capture());

    var capturedPtre = ptreCaptor.getValue();
    Assertions.assertEquals(PublicationStatus.ERROR, capturedPtre.getStatus());
    Assertions.assertEquals(HttpStatus.BAD_REQUEST.getReasonPhrase(), capturedPtre.getResponse());
    Assertions.assertEquals(HttpStatus.BAD_REQUEST.value(), capturedPtre.getResponseStatusCode());
  }

  @Test
  void checkPublicationDetailsExists() {
    var consortiumId = UUID.randomUUID();
    var publicationId = UUID.randomUUID();

    doNothing().when(consortiumService).checkConsortiumExistsOrThrow(consortiumId);
    when(publicationStatusRepository.existsById(publicationId)).thenReturn(true);

    publicationService.checkPublicationDetailsExists(consortiumId, publicationId);

    Mockito.verify(publicationStatusRepository).existsById(publicationId);
    Mockito.verify(consortiumService).checkConsortiumExistsOrThrow(consortiumId);
  }

  @Test
  void deletePublicationByIdSuccessful() {
    var consortiumId = UUID.randomUUID();
    var publicationId = UUID.randomUUID();

    doNothing().when(consortiumService).checkConsortiumExistsOrThrow(consortiumId);
    when(publicationStatusRepository.existsById(publicationId)).thenReturn(true);

    publicationService.deletePublicationById(consortiumId, publicationId);

    Mockito.verify(publicationStatusRepository).deleteById(publicationId);
    Mockito.verify(publicationTenantRequestRepository).deleteByPcStateId(publicationId);
  }

  @Test
  void deletePublicationByIdThrowsExceptionIfResourceDoesNotExist() {
    var consortiumId = UUID.randomUUID();
    var publicationId = UUID.randomUUID();

    doNothing().when(consortiumService).checkConsortiumExistsOrThrow(consortiumId);
    when(publicationStatusRepository.existsById(publicationId)).thenReturn(false);

    assertThrows(ResourceNotFoundException.class, () ->
      publicationService.deletePublicationById(consortiumId, publicationId));
  }

  @Test
  void publishRequest_negative_systemUserContextWithoutUserId() {
    var consortiumId = UUID.randomUUID();

    var publicationRequest = new PublicationRequest();
    publicationRequest.setTenants(Set.of(CENTRAL_TENANT_NAME));
    doNothing().when(tenantService).checkTenantsAndConsortiumExistsOrThrow(eq(consortiumId), any());
    when(userTenantService.userHasPrimaryAffiliationByUsernameAndTenantId(anyString(), eq(CENTRAL_TENANT_NAME))).thenReturn(false);
    when(folioExecutionContext.getUserId()).thenReturn(null);
    when(folioExecutionContext.getInstance()).thenReturn(folioExecutionContext);

    assertThrows(PublicationException.class, () ->
      publicationService.publishRequest(consortiumId, publicationRequest));
  }
}
