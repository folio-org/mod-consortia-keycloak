package org.folio.consortia.service;

import static org.folio.spring.scope.FolioExecutionScopeExecutionContextManager.getRunnableWithCurrentFolioContext;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.folio.consortia.domain.dto.PublicationDetailsResponse;
import org.folio.consortia.domain.dto.PublicationRequest;
import org.folio.consortia.domain.dto.PublicationResult;
import org.folio.consortia.domain.dto.PublicationStatus;
import org.folio.consortia.domain.dto.SourceValues;
import org.folio.consortia.domain.dto.Tenant;
import org.folio.consortia.domain.dto.TenantCollection;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.service.SystemUserScopedExecutionService;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.task.TaskExecutor;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.retry.RetryException;
import org.springframework.retry.backoff.FixedBackOffPolicy;
import org.springframework.retry.policy.SimpleRetryPolicy;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.transaction.annotation.Transactional;

@Log4j2
@RequiredArgsConstructor
public abstract class BaseSharingService<TRequest, TResponse, TDeleteResponse, TEntity> {

  protected static final String SOURCE = "source";
  protected static final String TYPE = "type";

  @Value("${folio.sharing.config.interval:200}")
  private int interval;
  @Value("${folio.sharing.config.max-tries:20}")
  private int maxTries;

  private final TenantService tenantService;
  private final ConsortiumService consortiumService;
  protected final SystemUserScopedExecutionService systemUserScopedExecutionService;
  private final PublicationService publicationService;
  protected final FolioExecutionContext folioExecutionContext;
  protected final ObjectMapper objectMapper;
  private final TaskExecutor asyncTaskExecutor;

  @Transactional
  public TResponse start(UUID consortiumId, TRequest request) {
    String configName = getClassName(request);
    UUID configId = getConfigId(request);
    log.debug("start:: Trying to share '{}' with consortiumId: {}, sharing {} id: {}",
      configName, consortiumId, configName, configId);

    consortiumService.checkConsortiumExistsOrThrow(consortiumId);
    checkEqualsOfPayloadIdWithConfigId(request);

    Set<String> sharedConfigTenants = findTenantsForConfig(request);
    TenantCollection allTenants = tenantService.getAll(consortiumId);

    // sync sharing config table, if there is already created config in tenant
    syncConfigWithTenants(sharedConfigTenants, request);
    sharedConfigTenants = findTenantsForConfig(request); // update sharedConfigTenants after sync

    List<PublicationRequest> pubPostRequests = new ArrayList<>();
    List<PublicationRequest> pubPutRequests = new ArrayList<>();

    List<TEntity> sharingConfigEntityList = new ArrayList<>();

    for (Tenant tenant : allTenants.getTenants()) {
      var method = sharedConfigTenants.contains(tenant.getId()) ? HttpMethod.PUT : HttpMethod.POST;
      var publicationRequest = buildPublicationRequestForTenant(request, tenant.getId(), method);

      if (method == HttpMethod.PUT) {
        pubPostRequests.add(publicationRequest);
      } else {
        pubPutRequests.add(publicationRequest);
        sharingConfigEntityList.add(createSharingConfigEntityFromRequest(request, tenant.getId()));
      }

      log.info("start:: tenant={} added to publication {} request for {}={}",
        tenant.getId(), method.toString(), getClassName(request), getConfigId(request));
    }

    saveSharingConfig(sharingConfigEntityList);
    log.info("start:: The Sharing {}s for {} ID '{}' and '{}' unique tenant(s) were successfully" +
      " saved to the database", configName, configName, configId, sharingConfigEntityList.size());

    var sourceValue = getSourceValue(SourceValues.CONSORTIUM);
    pubPostRequests.forEach(pubRequest -> updateSourcePayload(pubRequest, sourceValue));
    pubPutRequests.forEach(pubRequest -> updateSourcePayload(pubRequest, sourceValue));

    log.info("start:: set source as '{}' in payload of {}: {}",
      sourceValue, configName, configId);

    // create PC request with POST and PUT Http method to create configs, using 'mod-consortia-keycloak' system user
    return systemUserScopedExecutionService.executeSystemUserScoped(folioExecutionContext.getTenantId(), () -> {
      var createConfigsPcIds = executePublishRequests(consortiumId, pubPostRequests);
      var updateConfigsPcIds = executePublishRequests(consortiumId, pubPutRequests);

      return createSharingConfigResponse(createConfigsPcIds, updateConfigsPcIds);
    });
  }

  @Transactional
  @SneakyThrows
  public TDeleteResponse delete(UUID consortiumId, UUID configId, TRequest request) {
    String configName = getClassName(request);
    log.debug("delete:: Trying to delete sharing '{}' with consortiumId: {}, sharing {} ID: {}",
      configName, consortiumId, configName, configId);

    validateSharingConfigRequestOrThrow(configId, request);
    consortiumService.checkConsortiumExistsOrThrow(consortiumId);

    Set<String> sharedTenants = findTenantsForConfig(request);
    TenantCollection allTenants = tenantService.getAll(consortiumId);

    syncConfigWithTenants(sharedTenants, request);
    sharedTenants = findTenantsForConfig(request); // update sharedTenants after sync

    List<PublicationRequest> pubDeleteRequests = new ArrayList<>();

    for (Tenant tenant : allTenants.getTenants()) {
      if (sharedTenants.contains(tenant.getId())) {
        pubDeleteRequests.add(buildPublicationRequestForTenant(request, tenant.getId(), HttpMethod.DELETE));
      }
    }

    deleteSharingConfig(request);
    log.info("delete:: The Sharing {}s for {} ID '{}' and '{}' tenant(s) were successfully" +
      " deleted from the database", configName, configName, configId, sharedTenants);

    // create PC request with DELETE Http method to create configs, using 'mod-consortia-keycloak' system user
    return systemUserScopedExecutionService.executeSystemUserScoped(folioExecutionContext.getTenantId(), () -> {
      var pcIds = executePublishRequests(consortiumId, pubDeleteRequests);
      var sharingConfigDeleteResponse = createSharingConfigDeleteResponse(pcIds);

      // update sources of failed requests
      asyncTaskExecutor.execute(getRunnableWithCurrentFolioContext(() ->
        pcIds.forEach(pcId -> updateConfigsForFailedTenantsWithRetry(consortiumId, pcId, request))));

      return sharingConfigDeleteResponse;
    });
  }

  private void checkEqualsOfPayloadIdWithConfigId(TRequest sharingConfigRequest) {
    String sharingConfigId = String.valueOf(getConfigId(sharingConfigRequest));
    var payloadNode = objectMapper.convertValue(getPayload(sharingConfigRequest), ObjectNode.class);
    String payloadId = getPayloadId(payloadNode);
    if (ObjectUtils.notEqual(sharingConfigId, payloadId)) {
      throw new IllegalArgumentException("Mismatch ID in payload with ID");
    }
  }

  /**
   * Execute all publish request and return list of response uuids
   * @param consortiumId id of consortium
   * @param publicationRequests list of publication request
   * @return list of response uuids
   */
  private List<UUID> executePublishRequests(UUID consortiumId, List<PublicationRequest> publicationRequests) {
    return publicationRequests.stream()
      .map(publicationRequest -> publishRequest(consortiumId, publicationRequest))
      .toList();
  }

  private UUID publishRequest(UUID consortiumId, PublicationRequest publicationRequest) {
    if (CollectionUtils.isNotEmpty(publicationRequest.getTenants())) {
      log.info("publishRequest:: Sending {} request to publication with {} tenants for consortiumId={} and url={}",
        publicationRequest.getMethod(), publicationRequest.getTenants().size(), consortiumId, publicationRequest.getUrl());
      return publicationService.publishRequest(consortiumId, publicationRequest).getId();
    }

    log.info("publishRequest:: Tenant list of publishing for http method: {} is empty", publicationRequest.getMethod());
    return null;
  }

  /**
   * The method execute <code>updateConfigsForFailedTenants</code> method with retry.
   * Retry based on <code>maxTries</code>
   * Fixed backoff period based on <code>interval</code>
   *
   * @param consortiumId id of consortium
   * @param publicationId id of publication
   * @param sharingConfigRequest sharing config request
   */
  private void updateConfigsForFailedTenantsWithRetry(UUID consortiumId, UUID publicationId,
                                                      TRequest sharingConfigRequest) {
    if (publicationId == null) {
      return;
    }

    RetryTemplate retryTemplate = new RetryTemplate();

    SimpleRetryPolicy retryPolicy = new SimpleRetryPolicy(maxTries);
    retryTemplate.setRetryPolicy(retryPolicy);

    FixedBackOffPolicy backOffPolicy = new FixedBackOffPolicy();
    backOffPolicy.setBackOffPeriod(interval); // in milliseconds
    retryTemplate.setBackOffPolicy(backOffPolicy);

    retryTemplate.execute(context -> updateConfigsForFailedTenants(consortiumId, publicationId, sharingConfigRequest));
  }

  private boolean updateConfigsForFailedTenants(UUID consortiumId, UUID publicationId,
                                                TRequest sharingConfigRequest) {
    log.debug("updateConfigsForFailedTenants:: Trying to update {}s for failed tenants for consortiumId={}" +
        " publicationId={} and sharingConfigRequestId={}", getClassName(sharingConfigRequest), consortiumId,
      publicationId, getConfigId(sharingConfigRequest));

    boolean isPublicationStatusReady = isPublicationStatusReady(consortiumId, publicationId);
    if (isPublicationStatusReady) {
      Set<String> failedTenantList = getFailedTenantList(consortiumId, publicationId);
      log.info("updateConfigsForFailedTenants:: '{}' tenant(s) failed ", failedTenantList.size());

      if (ObjectUtils.isNotEmpty(failedTenantList)) {
        updateFailedConfigsToLocalSource(consortiumId, sharingConfigRequest, failedTenantList);
      }
      return true;
    } else {
      String errMsg = String.format("updateConfigsForFailedTenants:: Publication status is not ready or doesn't exist for " +
        "consortiumId=%s, publicationId=%s and sharingConfigRequestId=%s", consortiumId, publicationId, getConfigId(sharingConfigRequest));
      log.error(errMsg);
      throw new RetryException(errMsg);
    }
  }

  private boolean isPublicationStatusReady(UUID consortiumId, UUID publicationId) {
    boolean isPublicationStatusExists = publicationService.checkPublicationDetailsExists(consortiumId, publicationId);
    if (isPublicationStatusExists) {
      PublicationDetailsResponse publicationDetails =
        publicationService.getPublicationDetails(consortiumId, publicationId);
      return ObjectUtils.notEqual(publicationDetails.getStatus(), PublicationStatus.IN_PROGRESS);
    }
    return false;
  }

  private Set<String> getFailedTenantList(UUID consortiumId, UUID publicationId) {
    return publicationService.getPublicationResults(consortiumId, publicationId).getPublicationResults()
      .stream().filter(publicationResult -> HttpStatus.valueOf(publicationResult.getStatusCode()).isError())
      .map(PublicationResult::getTenantId).collect(Collectors.toSet());
  }

  private void updateFailedConfigsToLocalSource(UUID consortiumId, TRequest request,
                                                Set<String> failedTenantList) {
    log.info("updateFailedConfigsToLocalSource:: Updating failed '{}' tenants {}s ",
      failedTenantList.size(), getClassName(request));

    List<PublicationRequest> pubPutRequests = new ArrayList<>();
    failedTenantList.forEach(tenantId ->
      pubPutRequests.add(buildPublicationRequestForTenant(request,  tenantId, HttpMethod.PUT)));

    var sourceValue = getSourceValue(SourceValues.USER);
    var updatedPayload = updateSourcePayload(request, sourceValue);
    pubPutRequests.forEach(pubRequest -> pubRequest.setPayload(updatedPayload));

    log.info("updateFailedConfigsToLocalSource:: send PUT request to publication with new source in " +
        "payload={} by system user of {}", sourceValue, folioExecutionContext.getTenantId());
    executePublishRequests(consortiumId, pubPutRequests);
  }

  protected abstract UUID getConfigId(TRequest request);
  protected abstract Object getPayload(TRequest request);
  protected abstract String getPayloadId(ObjectNode payload);
  protected abstract String getUrl(TRequest request, HttpMethod httpMethod);
  protected abstract void validateSharingConfigRequestOrThrow(UUID configId, TRequest request);

  protected abstract void syncConfigWithTenants(Set<String> sharedConfigTenants, TRequest request);
  protected abstract Set<String> findTenantsForConfig(TRequest request);
  protected abstract void saveSharingConfig(List<TEntity> enetityList);
  protected abstract void deleteSharingConfig(TRequest request);

  protected abstract PublicationRequest buildPublicationRequestForTenant(TRequest request, String tenantId,
                                                                         HttpMethod method);
  protected abstract TEntity createSharingConfigEntityFromRequest(TRequest request, String tenantId);
  protected abstract TResponse createSharingConfigResponse(List<UUID> createConfigsPcId, List<UUID> updateConfigsPcId);
  protected abstract TDeleteResponse createSharingConfigDeleteResponse(List<UUID> publishRequestId);
  protected abstract String getSourceValue(SourceValues sourceValue);
  protected abstract ObjectNode updateSourcePayload(Object payload, String sourceValue);

  private String getClassName(TRequest sharingConfigRequest) {
    return sharingConfigRequest.getClass().getSimpleName();
  }
}
