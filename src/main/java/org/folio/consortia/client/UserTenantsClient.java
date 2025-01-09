package org.folio.consortia.client;

import org.folio.consortia.domain.dto.UserTenant;
import org.folio.consortia.domain.dto.UserTenantCollection;
import org.folio.spring.config.FeignClientConfiguration;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

@FeignClient(name = "user-tenants" , configuration = FeignClientConfiguration.class)
public interface UserTenantsClient {

  @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
  UserTenantCollection getUserTenants();

  @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
  void postUserTenant(@RequestBody UserTenant userTenant);

  @DeleteMapping
  void deleteUserTenants();
}
