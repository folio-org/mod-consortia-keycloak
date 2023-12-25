package org.folio.consortia.service;

import org.folio.consortia.domain.dto.PrimaryAffiliationEvent;
import org.folio.consortia.domain.entity.TenantEntity;

import java.util.UUID;

public interface PrimaryAffiliationService {
  /**
   * Creates primary affiliation and sends event to kafka using system user context.
   * If tenant is member - additional affiliation will be created in central tenant as well.
   * This method uses Sprint REQUIRES_NEW propagation type to create each affiliation in new transaction.
   * It can be useful in places that already have open transaction and creating affiliations for multiple users -
   * in this case if some particular affiliation was not created - the entire outer transaction will not be roll backed.
   *
   * @param consortiumId the consortium id
   * @param centralTenantId the central tenant id
   * @param tenantEntity the tenant entity
   * @param event the primary affiliation event to send to kafka
   */
  void createPrimaryAffiliationInNewTransaction(UUID consortiumId,
                                                String centralTenantId,
                                                TenantEntity tenantEntity,
                                                PrimaryAffiliationEvent event);

  /**
   * Creates primary affiliation and sends event to kafka using system user context.
   * If tenant is member - additional affiliation will be created in central tenant as well.
   * This method also transactional and joins already created outer transaction(if exists) or creates new one.
   * It can be useful in places that creates single affiliation like processing user domain events.
   *
   * @param consortiumId the consortium id
   * @param centralTenantId the central tenant id
   * @param tenantEntity the tenant entity
   * @param event the primary affiliation event to send to kafka
   */
  void createPrimaryAffiliation(UUID consortiumId,
                                String centralTenantId,
                                TenantEntity tenantEntity,
                                PrimaryAffiliationEvent event);
}
