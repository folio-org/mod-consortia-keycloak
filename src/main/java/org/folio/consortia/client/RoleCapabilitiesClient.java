package org.folio.consortia.client;


import com.bettercloud.vault.json.JsonObject;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(name = "role-capabilities")
public interface RoleCapabilitiesClient {

  @GetMapping(value = "/roles/{roleId}/capabilities")
  JsonObject getRoleCapabilitiesByRoleId(@PathVariable String roleId);
}
