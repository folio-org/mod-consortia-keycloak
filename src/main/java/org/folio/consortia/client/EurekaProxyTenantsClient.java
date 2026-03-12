package org.folio.consortia.client;

import java.net.URI;
import java.util.List;

import org.folio.consortia.domain.dto.ModuleForTenant;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;

@HttpExchange("eureka")
public interface EurekaProxyTenantsClient {

  @GetExchange(value = "/proxy/tenants/{tenantId}/modules", accept = MediaType.APPLICATION_JSON_VALUE)
  List<ModuleForTenant> getModules(URI uri, @RequestParam String tenantId);
}

