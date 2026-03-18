package org.folio.consortia.client;

import java.util.List;

import org.folio.consortia.domain.dto.ModuleForTenant;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;

@HttpExchange("http://_/proxy")
public interface EurekaProxyTenantsClient {

  @GetExchange(value = "/tenants/{tenantId}/modules", accept = MediaType.APPLICATION_JSON_VALUE)
  List<ModuleForTenant> getModules(@PathVariable String tenantId);
}

