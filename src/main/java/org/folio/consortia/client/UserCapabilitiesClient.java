package org.folio.consortia.client;

import org.folio.spring.config.FeignClientConfiguration;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(name = "user-capabilities-client", url = "users", configuration = FeignClientConfiguration.class)
public interface UserCapabilitiesClient {

  @DeleteMapping("/{userId}/capabilities")
  void deleteUserCapabilities(@PathVariable("userId") String userId);
}
