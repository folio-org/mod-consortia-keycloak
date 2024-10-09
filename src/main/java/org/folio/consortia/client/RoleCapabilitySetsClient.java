package org.folio.consortia.client;

import java.util.UUID;

import org.folio.consortia.domain.dto.CapabilitySets;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(name = "role-capability-sets")
public interface RoleCapabilitySetsClient {

  @GetMapping(value = "/roles/{roleId}/capability-sets")
  CapabilitySets getRoleCapabilitySetsRoleId(@PathVariable UUID roleId);
}
