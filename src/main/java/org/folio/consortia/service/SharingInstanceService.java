package org.folio.consortia.service;

import java.util.UUID;

import org.folio.consortia.domain.dto.SharingInstance;
import org.folio.consortia.domain.dto.SharingInstanceCollection;
import org.folio.consortia.domain.dto.Status;

public interface SharingInstanceService {

  /**
   * Get a sharing instance by action ID
   * @param consortiumId  id of consortium
   * @param actionId id of sharing instance
   * @return SharingInstanceDto
   */
  SharingInstance getById(UUID consortiumId, UUID actionId);

  /**
   * Start instance sharing action
   * @param consortiumId UUID of consortium entity
   * @param sharingInstance the sharingInstanceDto
   * @return SharingInstanceDto
   */
  SharingInstance start(UUID consortiumId, SharingInstance sharingInstance);

  /**
   * Get a sharing instance collection
   * @param consortiumId       the UUID of the consortium
   * @param instanceIdentifier the UUID of the instance
   * @param sourceTenantId     the ID of the source tenant
   * @param targetTenantId     the ID of the target tenant
   * @param status             the status of the sharing instance
   * @param offset             the offset
   * @param limit              the limit
   * @return the sharing instance collection
   */
  SharingInstanceCollection getSharingInstances(UUID consortiumId, UUID instanceIdentifier, String sourceTenantId,
      String targetTenantId, Status status, Integer offset, Integer limit);

  /**
   * Update 'status' and 'error' fields of sharingInstance according to Kafka message payload
   * @param promotingEvent contains 'instanceIdentifier', 'sourceTenantId', 'targetTenantId', and 'error'
   */
  void completePromotingLocalInstance(String promotingEvent);
}
