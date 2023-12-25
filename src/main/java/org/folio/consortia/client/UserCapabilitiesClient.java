package org.folio.consortia.client;

import org.folio.consortia.domain.dto.UserCapabilitiesRequest;
import org.folio.spring.config.FeignClientConfiguration;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;

@FeignClient(name = "user-capabilities-client", url = "users", configuration = FeignClientConfiguration.class)
public interface UserCapabilitiesClient {

  @PutMapping("/{userId}/capabilities")
  void assignUserCapabilities(@PathVariable("userId") String userId, @RequestBody UserCapabilitiesRequest request);

  @DeleteMapping("/{userId}/capabilities")
  void deleteUserCapabilities(@PathVariable("userId") String userId);
}
