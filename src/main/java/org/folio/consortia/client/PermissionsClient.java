package org.folio.consortia.client;

import org.folio.consortia.domain.dto.Permission;
import org.folio.consortia.domain.dto.PermissionUser;
import org.folio.consortia.domain.dto.PermissionUserCollection;
import org.folio.spring.config.FeignClientConfiguration;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "consortia-perms-client", url = "perms/users", configuration = FeignClientConfiguration.class)
public interface PermissionsClient {
  @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
  PermissionUserCollection get(@RequestParam("query") String query);

  @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  PermissionUser create(@RequestBody PermissionUser permissionUser);

  @PostMapping(value = "/{userId}/permissions?indexField=userId", consumes = MediaType.APPLICATION_JSON_VALUE)
  void addPermission(@PathVariable("userId") String userId, Permission permission);

  @DeleteMapping(value = "/{id}")
  void deletePermissionUser(@PathVariable("id") String id);
}
