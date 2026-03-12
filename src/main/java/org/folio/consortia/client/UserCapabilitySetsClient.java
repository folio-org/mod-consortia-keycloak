package org.folio.consortia.client;

import org.folio.consortia.domain.dto.UserCapabilitySetsRequest;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.service.annotation.DeleteExchange;
import org.springframework.web.service.annotation.HttpExchange;
import org.springframework.web.service.annotation.PutExchange;

@HttpExchange("users")
public interface UserCapabilitySetsClient {

  @PutExchange("/{userId}/capability-sets")
  void assignUserCapabilitySets(@PathVariable("userId") String userId, @RequestBody UserCapabilitySetsRequest request);

  @DeleteExchange("/{userId}/capability-sets")
  void deleteUserCapabilitySets(@PathVariable("userId") String userId);
}
