package org.folio.consortia.client;

import org.folio.consortia.domain.dto.UserTenant;
import org.folio.consortia.domain.dto.UserTenantCollection;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.service.annotation.DeleteExchange;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;
import org.springframework.web.service.annotation.PostExchange;

@HttpExchange("user-tenants")
public interface UserTenantsClient {

  @GetExchange(accept = MediaType.APPLICATION_JSON_VALUE)
  UserTenantCollection getUserTenants();

  @PostExchange(contentType = MediaType.APPLICATION_JSON_VALUE)
  void postUserTenant(@RequestBody UserTenant userTenant);

  @DeleteExchange
  void deleteUserTenants();

  @DeleteExchange
  void deleteUserTenantsByTenantId(@RequestParam("tenantId") String tenantId);
}
