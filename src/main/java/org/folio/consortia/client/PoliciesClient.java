package org.folio.consortia.client;

import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;

import java.util.UUID;

@HttpExchange("policies")
public interface PoliciesClient {

  @GetExchange(value = "/{policyId}")
  void getPolicyById(@PathVariable UUID policyId);
}
