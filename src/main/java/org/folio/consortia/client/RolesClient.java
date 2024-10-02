package org.folio.consortia.client;

import com.bettercloud.vault.json.JsonObject;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.UUID;

@FeignClient(name = "roles")
public interface RolesClient {

  @GetMapping(value = "/{roleId}")
  JsonObject getRoleById(@PathVariable UUID roleId);

  @GetMapping
  JsonObject getRolesByQuery(@RequestParam String query);

}
