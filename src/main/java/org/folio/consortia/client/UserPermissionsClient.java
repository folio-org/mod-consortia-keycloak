package org.folio.consortia.client;

import org.folio.consortia.domain.dto.PermissionUser;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "permissions")
public interface UserPermissionsClient {

  @GetMapping("/users/{id}")
  PermissionUser getPermissionsForUser(@PathVariable("id") String id, @RequestParam Boolean onlyVisible);
}
