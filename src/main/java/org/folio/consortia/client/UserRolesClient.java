package org.folio.consortia.client;

import org.folio.spring.config.FeignClientConfiguration;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(name = "roles-user", url = "/roles", configuration = FeignClientConfiguration.class)
public interface UserRolesClient {

  @DeleteMapping("/users/{id}")
  void deleteUserRoles(@PathVariable("userId") String userId);
}
