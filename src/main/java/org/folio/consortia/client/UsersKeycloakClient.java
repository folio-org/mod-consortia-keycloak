package org.folio.consortia.client;

import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.dto.UserIdpLinkingRequest;
import org.folio.spring.config.FeignClientConfiguration;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;

@FeignClient(name = "consortia-users-keycloak-client", url = "users-keycloak" , configuration = FeignClientConfiguration.class)
public interface UsersKeycloakClient {

  @GetMapping(value = "/users/{userId}")
  User getUsersByUserId(@PathVariable String userId);

  @PostMapping(value = "/users", consumes = MediaType.APPLICATION_JSON_VALUE)
  void saveUser(@RequestBody User user);

  @PutMapping(value = "/users/{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
  void updateUser(@PathVariable String id, @RequestBody User user);

  @DeleteMapping(value = "/users/{userId}")
  void deleteUser(@PathVariable String userId);

  @PostMapping(value = "/idp-migrations", consumes = MediaType.APPLICATION_JSON_VALUE)
  void createUsersIdpLinks(@RequestBody UserIdpLinkingRequest userIdpLinkingRequest);

}
