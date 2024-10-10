package org.folio.consortia.client;

import org.folio.consortia.domain.dto.UserCapabilitySetsRequest;
import org.folio.spring.config.FeignClientConfiguration;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;

@FeignClient(name = "user-capability-sets", url = "users", configuration = FeignClientConfiguration.class)
public interface UserCapabilitySetsClient {

  @PutMapping("/{userId}/capability-sets")
  void assignUserCapabilitySets(@PathVariable("userId") String userId, @RequestBody UserCapabilitySetsRequest request);

  @DeleteMapping("/{userId}/capability-sets")
  void deleteUserCapabilitySets(@PathVariable("userId") String userId);
}
