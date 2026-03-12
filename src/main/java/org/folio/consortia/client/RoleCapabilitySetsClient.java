package org.folio.consortia.client;

import java.util.UUID;

import org.folio.consortia.domain.dto.CapabilitySets;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;

@HttpExchange("roles")
public interface RoleCapabilitySetsClient {

  @GetExchange(value = "/{roleId}/capability-sets")
  CapabilitySets getRoleCapabilitySetsRoleId(@PathVariable UUID roleId);
}
