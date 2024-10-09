package org.folio.consortia.client;


import java.util.UUID;

import org.folio.consortia.domain.dto.Capabilities;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(name = "role-capabilities")
public interface RoleCapabilitiesClient {

  @GetMapping(value = "/roles/{roleId}/capabilities")
  Capabilities getRoleCapabilitiesByRoleId(@PathVariable UUID roleId);
}
