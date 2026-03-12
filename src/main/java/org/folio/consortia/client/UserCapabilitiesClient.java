package org.folio.consortia.client;

import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.service.annotation.DeleteExchange;
import org.springframework.web.service.annotation.HttpExchange;

@HttpExchange("users")
public interface UserCapabilitiesClient {

  @DeleteExchange("/{userId}/capabilities")
  void deleteUserCapabilities(@PathVariable("userId") String userId);
}
