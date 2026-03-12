package org.folio.consortia.client;

import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.dto.UsersIdpLinkOperationRequest;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.service.annotation.DeleteExchange;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;
import org.springframework.web.service.annotation.PostExchange;
import org.springframework.web.service.annotation.PutExchange;

@HttpExchange("users-keycloak")
public interface UsersKeycloakClient {

  @GetExchange(value = "/users/{userId}")
  User getUsersByUserId(@PathVariable String userId);

  @PostExchange(value = "/users", contentType = MediaType.APPLICATION_JSON_VALUE)
  void saveUser(@RequestBody User user);

  @PutExchange(value = "/users/{id}", contentType = MediaType.APPLICATION_JSON_VALUE)
  void updateUser(@PathVariable String id, @RequestBody User user);

  @DeleteExchange(value = "/users/{userId}")
  void deleteUser(@PathVariable String userId);

  @PostExchange(value = "/idp-migrations", contentType = MediaType.APPLICATION_JSON_VALUE)
  void createUsersIdpLinks(@RequestBody UsersIdpLinkOperationRequest usersIdpLinkOperationRequest);

  @DeleteExchange(value = "/idp-migrations", contentType = MediaType.APPLICATION_JSON_VALUE)
  void deleteUsersIdpLinks(@RequestBody UsersIdpLinkOperationRequest usersIdpLinkOperationRequest);

}
