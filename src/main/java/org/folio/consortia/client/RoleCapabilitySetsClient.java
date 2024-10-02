package org.folio.consortia.client;

import com.bettercloud.vault.json.JsonObject;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(name = "role-capability-sets")
public interface RoleCapabilitySetsClient {

  @GetMapping(value = "/roles/{roleId}/capability-sets")
  JsonObject getRoleCapabilitySetsRoleId(@PathVariable String roleId);
}
