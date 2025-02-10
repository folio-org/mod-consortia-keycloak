package org.folio.consortia.service;

import java.util.List;
import java.util.UUID;

import org.folio.consortia.domain.dto.SyncUser;

public interface CreatePrimaryAffiliationService {

  /**
   * Create affiliations between central tenant and users in the specified tenant
   *
   * @param consortiumId    - consortium unique identifier
   * @param centralTenantId - central tenant unique identifier
   * @param tenantId        - tenant unique identifier
   * @param users           - list of users to create affiliations for
   */
  void createPrimaryUserAffiliations(UUID consortiumId, String centralTenantId, String tenantId, List<SyncUser> users);

}
