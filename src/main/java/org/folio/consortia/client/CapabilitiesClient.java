package org.folio.consortia.client;

import org.folio.consortia.domain.dto.Capabilities;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "capabilities")
public interface CapabilitiesClient {

  @GetMapping
  Capabilities queryCapabilities(@RequestParam String query, @RequestParam Integer limit, @RequestParam Integer offset);
}
