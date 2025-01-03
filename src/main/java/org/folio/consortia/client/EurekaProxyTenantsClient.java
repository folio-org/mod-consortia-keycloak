package org.folio.consortia.client;

import java.net.URI;
import java.util.List;

import org.folio.consortia.domain.dto.ModuleForTenant;
import org.folio.spring.config.FeignClientConfiguration;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "eureka", configuration = FeignClientConfiguration.class)
public interface EurekaProxyTenantsClient {

  @GetMapping(value = "/proxy/tenants/{tenantId}/modules", produces = MediaType.APPLICATION_JSON_VALUE)
  List<ModuleForTenant> getModules(URI uri, @RequestParam String tenantId);
}
