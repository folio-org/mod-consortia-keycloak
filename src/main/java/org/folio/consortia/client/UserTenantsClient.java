package org.folio.consortia.client;

import org.folio.consortia.domain.dto.UserTenant;
import org.folio.spring.config.FeignClientConfiguration;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

@FeignClient(name = "user-tenants" , configuration = FeignClientConfiguration.class)
public interface UserTenantsClient {

  @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
  void postUserTenant(@RequestBody UserTenant userTenant);
}
