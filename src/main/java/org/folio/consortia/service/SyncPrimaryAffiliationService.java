package org.folio.consortia.service;

import org.folio.consortia.domain.dto.SyncPrimaryAffiliationBody;

import java.util.UUID;

public interface SyncPrimaryAffiliationService {

  /**
   * Sync primary affiliation for user
   * @param consortiumId               - consortium unique identifier
   * @param centralTenantId            - central tenant unique identifier
   * @param syncPrimaryAffiliationBody - consortia tenant record
   */
  void syncPrimaryAffiliations(UUID consortiumId, String centralTenantId, String syncPrimaryAffiliationBody);

  /**
   * Create affiliations between central tenant and user
   *        primary affiliation between local tenant and its user
   * @param consortiumId               - consortium unique identifier
   * @param syncPrimaryAffiliationBody - consortia tenant record
   */
  void createPrimaryUserAffiliations(UUID consortiumId, String centralTenantId, SyncPrimaryAffiliationBody syncPrimaryAffiliationBody);
}
