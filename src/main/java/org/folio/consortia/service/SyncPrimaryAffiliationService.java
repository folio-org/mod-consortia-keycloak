package org.folio.consortia.service;

import org.folio.consortia.domain.dto.SyncPrimaryAffiliationBody;

import java.util.UUID;

public interface SyncPrimaryAffiliationService {

  /**
   * Sync primary affiliation for all users in the tenant
   *
   * @param consortiumId    - consortium unique identifier
   * @param tenantId        - tenant unique identifier
   * @param centralTenantId - central tenant unique identifier
   */
  void syncPrimaryAffiliations(UUID consortiumId, String tenantId, String centralTenantId);

  /**
   * Create affiliations between central tenant and primary users in the tenant specified in the body
   *
   * @param consortiumId               - consortium unique identifier
   * @param centralTenantId            - central tenant unique identifier
   * @param syncPrimaryAffiliationBody - object with tenant id and user list
   */
  void createPrimaryUserAffiliations(UUID consortiumId, String centralTenantId, SyncPrimaryAffiliationBody syncPrimaryAffiliationBody);
}
