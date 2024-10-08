package org.folio.consortia.client;


import org.folio.consortia.domain.dto.Policies;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

import java.util.UUID;

@FeignClient(name = "policies")
public interface PoliciesClient {

  @GetMapping(value = "/{policyId}")
  Policies getPolicyById(@PathVariable UUID policyId);

}
