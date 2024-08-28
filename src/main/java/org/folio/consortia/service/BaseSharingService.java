package org.folio.consortia.service;

import static org.folio.spring.scope.FolioExecutionScopeExecutionContextManager.getRunnableWithCurrentFolioContext;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import com.fasterxml.jackson.databind.JsonNode;
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

  @Value("${folio.sharing.config.interval:200}")
  private int interval;
  @Value("${folio.sharing.config.max-tries:20}")
  private int maxTries;

  private final TenantService tenantService;
  private final ConsortiumService consortiumService;
  private final SystemUserScopedExecutionService systemUserScopedExecutionService;
  private final PublicationService publicationService;
  private final FolioExecutionContext folioExecutionContext;
  protected final ObjectMapper objectMapper;
  private final TaskExecutor asyncTaskExecutor;

  @Transactional
  public TResponse start(UUID consortiumId, TRequest sharingConfigRequest) {
    String configName = getClassName(sharingConfigRequest);
    UUID configId = getConfigId(sharingConfigRequest);
    log.debug("start:: Trying to share '{}' with consortiumId: {}, sharing {} id: {}",
      configName, consortiumId, configName, configId);

    consortiumService.checkConsortiumExistsOrThrow(consortiumId);
    checkEqualsOfPayloadIdWithConfigId(sharingConfigRequest);

    Set<String> sharingConfigTenants = findTenantsByConfigId(configId);
    TenantCollection allTenants = tenantService.getAll(consortiumId);

    var publicationPostRequest = createPublicationRequest(sharingConfigRequest, HttpMethod.POST.toString());
    var publicationPutRequest = createPublicationRequest(sharingConfigRequest, HttpMethod.PUT.toString());

    List<TEntity> sharingConfigEntityList = linkTenantsToPublicationPutPostRequestAndEntity(allTenants,
      sharingConfigRequest, sharingConfigTenants, publicationPutRequest, publicationPostRequest);
    saveSharingConfig(sharingConfigEntityList);
    log.info("start:: The Sharing {}s for {} ID '{}' and '{}' unique tenant(s) were successfully" +
      " saved to the database", configName, configName, configId, publicationPostRequest.getTenants().size());

    ObjectNode updatedPayload = updatePayload(sharingConfigRequest, SourceValues.CONSORTIUM.getValue());
    publicationPostRequest.setPayload(updatedPayload);
    publicationPutRequest.setPayload(updatedPayload);
    log.info("start:: set source as '{}' in payload of {}: {}",
      updatedPayload.get(SourceValues.CONSORTIUM.getValue()), configName, configId);

    // create PC request with POST and PUT Http method to create configs, using 'mod-consortia-keycloak' system user
    return systemUserScopedExecutionService.executeSystemUserScoped(folioExecutionContext.getTenantId(), () -> {
      UUID createConfigsPcId = publishRequest(consortiumId, publicationPostRequest);
      UUID updateConfigsPcId = publishRequest(consortiumId, publicationPutRequest);
      return createSharingConfigResponse(createConfigsPcId, updateConfigsPcId);
    });
  }


  @Transactional
  @SneakyThrows
  public TDeleteResponse delete(UUID consortiumId, UUID configId, TRequest sharingConfigRequest) {
    String configName = getClassName(sharingConfigRequest);
    log.debug("delete:: Trying to delete sharing '{}' with consortiumId: {}, sharing {} ID: {}",
      configName, consortiumId, configName, configId);

    validateSharingConfigRequestOrThrow(configId, sharingConfigRequest);
    consortiumService.checkConsortiumExistsOrThrow(consortiumId);

    Set<String> sharingConfigTenants = findTenantsByConfigId(configId);
    TenantCollection allTenants = tenantService.getAll(consortiumId);
    var publicationDeleteRequest = createPublicationRequest(sharingConfigRequest, HttpMethod.DELETE.toString());
    linkTenantsToPublicationDeleteRequest(allTenants, sharingConfigRequest, sharingConfigTenants, publicationDeleteRequest);
    log.info("delete:: Tenants with size: {} successfully added to appropriate DELETE publication " +
      "request for {}: {}", allTenants.getTotalRecords(), configName, configId);

    deleteSharingConfig(configId);
    log.info("delete:: The Sharing {}s for {} ID '{}' and '{}' unique tenant(s) were successfully " +
      "deleted from the database", configName, configName, configId, publicationDeleteRequest.getTenants().size());

    // create PC request with DELETE Http method to create configs, using 'mod-consortia-keycloak' system user
    return systemUserScopedExecutionService.executeSystemUserScoped(folioExecutionContext.getTenantId(), () -> {
      var pcId = publishRequest(consortiumId, publicationDeleteRequest);
      var sharingConfigDeleteResponse = createSharingConfigResponse(pcId);

      // update sources of failed requests
      asyncTaskExecutor.execute(getRunnableWithCurrentFolioContext(() ->
        updateConfigsForFailedTenantsWithRetry(consortiumId, pcId, sharingConfigRequest)));

      return sharingConfigDeleteResponse;
    });
  }

  private String getClassName(TRequest sharingConfigRequest) {
    return sharingConfigRequest.getClass().getName();
  }

  private void checkEqualsOfPayloadIdWithConfigId(TRequest sharingConfigRequest) {
    String sharingConfigId = String.valueOf(getConfigId(sharingConfigRequest));
    JsonNode payloadNode = objectMapper.convertValue(getPayload(sharingConfigRequest), JsonNode.class);
    String payloadId = payloadNode.get("id").asText();
    if (ObjectUtils.notEqual(sharingConfigId, payloadId)) {
      throw new IllegalArgumentException("Mismatch ID in payload with ID");
    }
  }

  /**
   * Method traverse through all tenants in db.
   * It will add tenant to 'PUT' method publication tenant list if it exists in config tenant associations.
   * Otherwise, it will add it to 'POST' method publication tenant list and add to sharingConfigEntityList
   *
   * @param allTenants             all existing tenants in db
   * @param sharingConfigRequest   sharing config request
   * @param sharingConfigTenants   existing tenants in configs
   * @param publicationPutRequest  publication put request
   * @param publicationPostRequest publication post request
   * @return List of SharingConfigEntity objects
   */
  private List<TEntity> linkTenantsToPublicationPutPostRequestAndEntity(TenantCollection allTenants,
                                                                        TRequest sharingConfigRequest,
                                                                        Set<String> sharingConfigTenants,
                                                                        PublicationRequest publicationPutRequest,
                                                                        PublicationRequest publicationPostRequest) {
    List<TEntity> sharingConfigEntityList = new ArrayList<>();
    for (Tenant tenant : allTenants.getTenants()) {
      if (sharingConfigTenants.contains(tenant.getId())) {
        publicationPutRequest.getTenants().add(tenant.getId());
        log.info("linkTenantsToPublicationPutPostRequestAndEntity:: tenant={} added to publication update request for {}={}",
          tenant.getId(), getClassName(sharingConfigRequest), getConfigId(sharingConfigRequest));
      } else {
        publicationPostRequest.getTenants().add(tenant.getId());
        log.info("linkTenantsToPublicationPutPostRequestAndEntity:: tenant={} added to publication create request for {}={}",
          tenant.getId(), getClassName(sharingConfigRequest), getConfigId(sharingConfigRequest));
        sharingConfigEntityList.add(createSharingConfigEntityFromRequest(sharingConfigRequest, tenant.getId()));
      }
    }
    return sharingConfigEntityList;
  }

  /**
   * Method traverse through all tenants in db.
   * It will add a tenant to delete a request publication tenant list
   * if it exists in config tenant associations
   *
   * @param allTenants               all existing tenants in db
   * @param sharingConfigRequest     sharing config request
   * @param sharingConfigTenants     existing tenants in configs
   * @param publicationDeleteRequest publication delete request
   */
  private void linkTenantsToPublicationDeleteRequest(TenantCollection allTenants,
                                                     TRequest sharingConfigRequest,
                                                     Set<String> sharingConfigTenants,
                                                     PublicationRequest publicationDeleteRequest) {

    for (Tenant tenant : allTenants.getTenants()) {
      if (sharingConfigTenants.contains(tenant.getId())) {
        publicationDeleteRequest.getTenants().add(tenant.getId());
        log.info("linkTenantsToPublicationDeleteRequest:: tenant={} added to publication delete request for {}={}",
          tenant.getId(), getClassName(sharingConfigRequest), getConfigId(sharingConfigRequest));
      }
    }
  }


  private UUID publishRequest(UUID consortiumId, PublicationRequest publicationRequest) {
    if (CollectionUtils.isNotEmpty(publicationRequest.getTenants())) {
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

  private void updateFailedConfigsToLocalSource(UUID consortiumId, TRequest sharingConfigRequest,
                                                Set<String> failedTenantList) {
    log.info("updateFailedConfigsToLocalSource:: Updating failed '{}' tenants {}s ",
      failedTenantList.size(), getClassName(sharingConfigRequest));
    ObjectNode updatedPayload = updatePayload(sharingConfigRequest, SourceValues.USER.getValue());

    PublicationRequest publicationPutRequest = createPublicationRequest(sharingConfigRequest, HttpMethod.PUT.toString());
    publicationPutRequest.setPayload(updatedPayload);
    publicationPutRequest.setTenants(failedTenantList);

    log.info("updateFailedConfigsToLocalSource:: send PUT request to publication with new source in " +
        "payload={} by system user of {}", SourceValues.USER.getValue(), folioExecutionContext.getTenantId());
    publishRequest(consortiumId, publicationPutRequest);
  }

  protected abstract UUID getConfigId(TRequest sharingConfigRequest);
  protected abstract Object getPayload(TRequest sharingConfigRequest);

  protected abstract void validateSharingConfigRequestOrThrow(UUID configId, TRequest sharingConfigRequest);

  protected abstract Set<String> findTenantsByConfigId(UUID configId);
  protected abstract void saveSharingConfig(List<TEntity> sharingConfigEntityList);
  protected abstract void deleteSharingConfig(UUID configId);

  protected abstract PublicationRequest createPublicationRequest(TRequest sharingConfigRequest, String httpMethod);
  protected abstract TEntity createSharingConfigEntityFromRequest(TRequest sharingConfigRequest, String tenantId);
  protected abstract TResponse createSharingConfigResponse(UUID createConfigsPcId, UUID updateConfigsPcId);
  protected abstract TDeleteResponse createSharingConfigResponse(UUID publishRequestId);
  protected abstract ObjectNode updatePayload(TRequest sharingConfigRequest, String sourceValue);

}
