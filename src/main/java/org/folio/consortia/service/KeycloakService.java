package org.folio.consortia.service;


import org.folio.consortia.domain.dto.Tenant;

public interface KeycloakService {
  void addCustomAuthFlowForCentralTenant(Tenant tenant);
}
