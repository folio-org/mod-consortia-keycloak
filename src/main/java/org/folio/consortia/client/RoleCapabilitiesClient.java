package org.folio.consortia.client;

import java.util.UUID;

import org.folio.consortia.domain.dto.Capabilities;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;

@HttpExchange("roles")
public interface RoleCapabilitiesClient {

  @GetExchange(value = "/{roleId}/capabilities")
  Capabilities getRoleCapabilitiesByRoleId(@PathVariable UUID roleId);
}
