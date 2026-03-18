package org.folio.consortia.client;

import org.folio.consortia.domain.dto.CapabilitySets;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;

@HttpExchange("capability-sets")
public interface CapabilitySetsClient {

  @GetExchange
  CapabilitySets queryCapabilitySets(@RequestParam String query, @RequestParam Integer limit, @RequestParam Integer offset);
}
