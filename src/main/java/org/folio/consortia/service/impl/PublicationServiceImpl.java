package org.folio.consortia.service.impl;

import static org.folio.spring.scope.FolioExecutionScopeExecutionContextManager.getRunnableWithCurrentFolioContext;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.http.HttpException;
import org.folio.consortia.domain.dto.PublicationDetailsResponse;
import org.folio.consortia.domain.dto.PublicationHttpResponse;
import org.folio.consortia.domain.dto.PublicationRequest;
import org.folio.consortia.domain.dto.PublicationResponse;
import org.folio.consortia.domain.dto.PublicationResult;
import org.folio.consortia.domain.dto.PublicationResultCollection;
import org.folio.consortia.domain.dto.PublicationStatus;
import org.folio.consortia.domain.dto.PublicationStatusError;
import org.folio.consortia.domain.entity.PublicationStatusEntity;
import org.folio.consortia.domain.entity.PublicationTenantRequestEntity;
import org.folio.consortia.exception.PublicationException;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.PublicationStatusRepository;
import org.folio.consortia.repository.PublicationTenantRequestRepository;
import org.folio.consortia.service.ConsortiumService;
import org.folio.consortia.service.HttpRequestService;
import org.folio.consortia.service.PublicationService;
import org.folio.consortia.service.TenantService;
import org.folio.consortia.service.UserTenantService;
import org.folio.consortia.utils.TenantContextUtils;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.data.OffsetRequest;
import org.folio.spring.scope.FolioExecutionContextSetter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.task.TaskExecutor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.HttpClientErrorException;

@Service
@Log4j2
@RequiredArgsConstructor
public class PublicationServiceImpl implements PublicationService {
  private static final String PUBLICATION_ID_FIELD = "publicationId";

  private final TenantService tenantService;
  private final UserTenantService userTenantService;
  private final FolioExecutionContext folioExecutionContext;
  private final FolioModuleMetadata folioModuleMetadata;
  private final HttpRequestService httpRequestService;
  private final TaskExecutor asyncTaskExecutor;

  private final PublicationStatusRepository publicationStatusRepository;
  private final PublicationTenantRequestRepository publicationTenantRequestRepository;
  private final ObjectMapper objectMapper;
  private final ConsortiumService consortiumService;

  @Value("${folio.max-active-threads:5}")
  private int maxActiveThreads;

  @Override
  @SneakyThrows
  public PublicationResponse publishRequest(UUID consortiumId, PublicationRequest publicationRequest) {
    validatePublicationRequest(consortiumId, publicationRequest, folioExecutionContext);

    PublicationStatusEntity createdPublicationEntity = createPublicationStatusEntity(publicationRequest.getTenants().size());
    var savedPublicationEntity = publicationStatusRepository.save(createdPublicationEntity);
    log.info("publishRequest:: Publication with id {} and status {} was created",
      savedPublicationEntity.getId(), savedPublicationEntity.getStatus());

    asyncTaskExecutor.execute(getRunnableWithCurrentFolioContext(
      () -> processTenantRequests(publicationRequest, savedPublicationEntity)));

    return buildPublicationResponse(savedPublicationEntity.getId());
  }

  @Override
  public PublicationDetailsResponse getPublicationDetails(UUID consortiumId, UUID publicationId) {
    log.debug("getPublicationDetails:: Trying to retrieve publication details by consortiumId: {} and publicationId id: {}", consortiumId, publicationId);

    consortiumService.checkConsortiumExistsOrThrow(consortiumId);
    var publicationStatusEntity = publicationStatusRepository.findById(publicationId)
      .orElseThrow(() -> new ResourceNotFoundException(PUBLICATION_ID_FIELD, String.valueOf(publicationId)));

    var ptrEntities = publicationTenantRequestRepository.findByPcStateId(publicationId, OffsetRequest.of(0, Integer.MAX_VALUE));
    log.info("getPublicationDetails:: Found {} of {} expected tenant request records", ptrEntities.getTotalElements(), publicationStatusEntity.getTotalRecords());

    var errorList = buildErrorListFromPublicationTenantRequestEntities(ptrEntities);
    var tenantRequestPayload = getPayloadFromPublicationTenantRequestEntities(ptrEntities);

    var pdr = new PublicationDetailsResponse()
      .id(publicationStatusEntity.getId())
      .status(publicationStatusEntity.getStatus())
      .dateTime(publicationStatusEntity.getCreatedDate().toString())
      .request(tenantRequestPayload)
      .errors(errorList);
    log.info("getPublicationDetails:: Prepared publication details response with id={}, status={}", pdr.getId(), pdr.getStatus());
    return pdr;
  }

  private String getPayloadFromPublicationTenantRequestEntities(Page<PublicationTenantRequestEntity> publicationTenantRequestEntity) {
    return publicationTenantRequestEntity.getContent()
      .stream()
      .map(PublicationTenantRequestEntity::getRequestPayload)
      .findFirst()
      .orElse(null);
  }

  private List<PublicationStatusError> buildErrorListFromPublicationTenantRequestEntities(Page<PublicationTenantRequestEntity> publicationTenantRequestEntity) {
    return publicationTenantRequestEntity.getContent()
      .stream()
      .filter(ptrEntity -> ptrEntity.getStatus() == PublicationStatus.ERROR)
      .map(ptrEntity -> new PublicationStatusError()
        .errorMessage(ptrEntity.getResponse())
        .errorCode(ptrEntity.getResponseStatusCode())
      .tenantId(ptrEntity.getTenantId()))
      .toList();
  }

  void processTenantRequests(PublicationRequest publicationRequest, PublicationStatusEntity createdPublicationEntity) {
    List<Future<PublicationTenantRequestEntity>> futures = new ArrayList<>();
    ExecutorService executor = Executors.newFixedThreadPool(maxActiveThreads);

    try {
      for (String tenantId : publicationRequest.getTenants()) {
        try {
          PublicationTenantRequestEntity ptrEntity = buildPublicationRequestEntity(publicationRequest, createdPublicationEntity, tenantId);
          var savedPublicationTenantRequest = savePublicationTenantRequest(ptrEntity);
          var future = executor.submit(() -> executeAndUpdatePublicationTenantRequest(publicationRequest, tenantId, savedPublicationTenantRequest));
          futures.add(future);
        } catch (Exception e) {
          log.error("processTenantRequests:: failed to create and submit task for tenant {} and publication id {} ",
            tenantId, createdPublicationEntity.getId(), e);
          futures.add(CompletableFuture.failedFuture(e));
        }
      }
    } finally {
      executor.shutdown();
    }

    try {
      if (!executor.awaitTermination(300, TimeUnit.SECONDS)) {
        log.warn("processTenantRequests:: Publication tasks timed out. Forcing shutdown. Publication id: {}", createdPublicationEntity.getId());
        executor.shutdownNow();
      }
    } catch (InterruptedException e) {
      log.error("Publication task executor was interrupted. Forcing shutdown. Publication id: {}", createdPublicationEntity.getId(), e);
      executor.shutdownNow();
      Thread.currentThread().interrupt();
    } finally {
      updatePublicationsStatus(futures, createdPublicationEntity);
    }
  }

  PublicationTenantRequestEntity executeAndUpdatePublicationTenantRequest(PublicationRequest publicationRequest, String tenantId,
    PublicationTenantRequestEntity savedPublicationTenantRequest) {
    PublicationTenantRequestEntity updatedPtre;
    try {
      var response = executeHttpRequest(publicationRequest, tenantId, folioExecutionContext);
      updatedPtre = updateSucceedPublicationTenantRequest(response, savedPublicationTenantRequest);
    } catch (Exception e) {
      updatedPtre = updateFailedPublicationTenantRequest(e, savedPublicationTenantRequest);
    }
    return updatedPtre;
  }

  private PublicationTenantRequestEntity savePublicationTenantRequest(PublicationTenantRequestEntity ptrEntity) {
   try {
      log.info("savePublicationTenantRequest:: PublicationTenantRequest with id '{}' and PublicationStatus with id '{} and status '{} was saved",
        ptrEntity.getId(), ptrEntity.getPcState().getId(), ptrEntity.getPcState().getStatus());
      return publicationTenantRequestRepository.save(ptrEntity);
    } catch (RuntimeException e) {
      log.error("savePublicationTenantRequest:: error saving publication tenant request {}", ptrEntity.getId(), e);
      throw new PublicationException(e);
    }
  }

  PublicationHttpResponse executeHttpRequest(PublicationRequest publicationRequest, String tenantId, FolioExecutionContext centralTenantContext) {
    try (var ignored = new FolioExecutionContextSetter(
      TenantContextUtils.prepareContextForTenant(tenantId, folioModuleMetadata, centralTenantContext))) {
      var response = httpRequestService.performRequest(publicationRequest.getUrl(), HttpMethod.valueOf(publicationRequest.getMethod()), publicationRequest.getPayload());
      if (response.getStatusCode().is2xxSuccessful()) {
        log.info("executeHttpRequest:: successfully called {} on tenant {}", publicationRequest.getUrl(), tenantId);
        return response;
      } else {
        var errMessage = response.getBody() != null ? response.getBody() : "Generic Error";
        log.error("executeHttpRequest:: error making {} '{}' request on tenant {}", publicationRequest.getMethod(), publicationRequest.getUrl(), tenantId, new HttpException(errMessage));
        throw new HttpClientErrorException(response.getStatusCode(), errMessage);
      }
    } catch (HttpClientErrorException e) {
      log.error("executeHttpRequest:: error making {} '{}' request on tenant {}", publicationRequest.getMethod(), publicationRequest.getUrl(), tenantId, e);
      throw new HttpClientErrorException(e.getStatusCode(), e.getMessage());
    }
  }

  PublicationTenantRequestEntity updateSucceedPublicationTenantRequest(PublicationHttpResponse responseEntity, PublicationTenantRequestEntity ptrEntity) {
    var currentLocalDateTime = LocalDateTime.now();
    ptrEntity.setCompletedDate(currentLocalDateTime);

    ptrEntity.setResponseStatusCode(responseEntity.getStatusCode().value());
    ptrEntity.setResponse(responseEntity.getBody());
    ptrEntity.setStatus(PublicationStatus.COMPLETE);

    return savePublicationTenantRequest(ptrEntity);
  }

  PublicationTenantRequestEntity updateFailedPublicationTenantRequest(Throwable t, PublicationTenantRequestEntity ptrEntity) {
    var currentLocalDateTime = LocalDateTime.now();
    ptrEntity.setCompletedDate(currentLocalDateTime);
    ptrEntity.setStatus(PublicationStatus.ERROR);
    if (t.getCause() instanceof HttpClientErrorException httpClientErrorException) {
      ptrEntity.setResponseStatusCode(httpClientErrorException.getStatusCode().value());
      ptrEntity.setResponse(httpClientErrorException.getStatusText());
    } else {
      ptrEntity.setResponseStatusCode(HttpStatus.BAD_REQUEST.value());
      ptrEntity.setResponse(t.getMessage());
    }

    return savePublicationTenantRequest(ptrEntity);
  }

  private PublicationTenantRequestEntity buildPublicationRequestEntity(PublicationRequest publicationRequest,
      PublicationStatusEntity savedPublicationEntity, String tenantId) throws JsonProcessingException {
    PublicationTenantRequestEntity ptrEntity = new PublicationTenantRequestEntity();
    String payload = objectMapper.writeValueAsString(publicationRequest.getPayload());

    ptrEntity.setId(UUID.randomUUID());
    ptrEntity.setRequestUrl(publicationRequest.getUrl());
    ptrEntity.setRequestPayload(payload);
    ptrEntity.setTenantId(tenantId);
    ptrEntity.setStatus(PublicationStatus.IN_PROGRESS);
    ptrEntity.setPcState(savedPublicationEntity);

    return ptrEntity;
  }

  private PublicationStatusEntity createPublicationStatusEntity(int totalRecords) {
    PublicationStatusEntity publicationStatusEntity = new PublicationStatusEntity();
    publicationStatusEntity.setId(UUID.randomUUID());
    publicationStatusEntity.setStatus(PublicationStatus.IN_PROGRESS);
    publicationStatusEntity.setTotalRecords(totalRecords);
    return publicationStatusEntity;
  }

  private void updatePublicationsStatus(List<Future<PublicationTenantRequestEntity>> futures, PublicationStatusEntity publicationStatusEntity) {
    List<PublicationTenantRequestEntity> ptreList = new ArrayList<>();
    futures.forEach(future -> {
      try {
        var ptre = future.get();
        ptreList.add(ptre);
      } catch (InterruptedException e) {
        // Will never occur. All executor threads are safely completed at this point
        Thread.currentThread().interrupt();
      } catch (ExecutionException e) {
        log.error("updatePublicationsStatus:: publication tenant request failed", e);
      }
    });
    var isCompletedWithExceptions = futures.size() != ptreList.size();
    var hasErrorStatus = ptreList.stream()
      .map(PublicationTenantRequestEntity::getStatus)
      .anyMatch(status -> status.equals(PublicationStatus.ERROR));
    var isErrorStatus = isCompletedWithExceptions || hasErrorStatus || futures.isEmpty();

    var updateStatus = isErrorStatus ? PublicationStatus.ERROR : PublicationStatus.COMPLETE;
    publicationStatusEntity.setStatus(updateStatus);

    publicationStatusRepository.save(publicationStatusEntity);
    log.info("updatePublicationsStatus:: updated publication record {} with status {}", publicationStatusEntity.getId(), publicationStatusEntity.getStatus());
  }

  protected void validatePublicationRequest(UUID consortiumId, PublicationRequest publication, FolioExecutionContext context) {
    if (CollectionUtils.isEmpty(publication.getTenants())) {
      throw new PublicationException(PublicationException.TENANT_LIST_EMPTY);
    }
    tenantService.checkTenantsAndConsortiumExistsOrThrow(consortiumId, List.copyOf(publication.getTenants()));

    // condition to support eureka system user approach
    if (context.getUserId() == null) {
      log.info("validatePublicationRequest:: skipping validating primary user affiliation for system user");
      return;
    }

    var userAffiliated = userTenantService.checkUserIfHasPrimaryAffiliationByUserId(consortiumId, context.getUserId().toString());
    if (!userAffiliated) {
      throw new PublicationException(PublicationException.PRIMARY_AFFILIATION_NOT_EXISTS);
    }
  }

  private PublicationResponse buildPublicationResponse(UUID publicationId) {
    return new PublicationResponse()
      .id(publicationId)
      .status(PublicationStatus.IN_PROGRESS);
  }


  @Override
  public PublicationResultCollection getPublicationResults(UUID consortiumId, UUID publicationId){
    log.info("getPublicationResults:: Trying to retrieve publication results by consortiumId: {} and publicationId id: {}", consortiumId, publicationId);

    consortiumService.checkConsortiumExistsOrThrow(consortiumId);
    var publicationStatusEntity = publicationStatusRepository.findById(publicationId)
      .orElseThrow(() -> new ResourceNotFoundException(PUBLICATION_ID_FIELD, String.valueOf(publicationId)));

    var ptrEntities = publicationTenantRequestRepository.findByPcStateId(publicationId, OffsetRequest.of(0, Integer.MAX_VALUE));
    log.info("getPublicationResults:: Found {} of {} expected tenant request records for publication {}", ptrEntities.getTotalElements(), publicationStatusEntity.getTotalRecords(), publicationId);

    var resultList = ptrEntities.stream()
      .map(entity -> new PublicationResult()
        .tenantId(entity.getTenantId())
        .statusCode(entity.getResponseStatusCode())
        .response(entity.getResponse()))
      .toList();

    return new PublicationResultCollection()
      .publicationResults(resultList)
      .totalRecords(resultList.size());
  }

  @Override
  public boolean checkPublicationDetailsExists(UUID consortiumId, UUID publicationId) {
    consortiumService.checkConsortiumExistsOrThrow(consortiumId);
    return publicationStatusRepository.existsById(publicationId);
  }

  @Override
  @Transactional
  public void deletePublicationById(UUID consortiumId, UUID publicationId) {
    log.debug("deletePublicationById:: Trying to delete publication by consortiumId: {} and publicationId id: {}", consortiumId, publicationId);

    consortiumService.checkConsortiumExistsOrThrow(consortiumId);
    if (!publicationStatusRepository.existsById(publicationId)) {
      throw new ResourceNotFoundException(PUBLICATION_ID_FIELD, String.valueOf(publicationId));
    }

    publicationTenantRequestRepository.deleteByPcStateId(publicationId);
    publicationStatusRepository.deleteById(publicationId);
    log.info("deletePublicationById:: Deleted publication by consortiumId: {} and publicationId id: {}", consortiumId, publicationId);
  }
}
