package org.folio.consortia.client;

import com.bettercloud.vault.json.JsonObject;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

import java.util.UUID;

@FeignClient(name = "policies")
public interface PoliciesClient {

  @GetMapping(value = "/{policyId}")
  JsonObject getPolicyById(@PathVariable UUID policyId);

}
