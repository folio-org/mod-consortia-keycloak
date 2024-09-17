package org.folio.consortia.client;

import org.folio.consortia.domain.dto.CapabilitySets;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "capability-sets")
public interface CapabilitySetsClient {

  @GetMapping
  CapabilitySets queryCapabilitySets(@RequestParam String query, @RequestParam Integer limit, @RequestParam Integer offset);
}
