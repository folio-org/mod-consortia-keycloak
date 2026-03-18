package org.folio.consortia.client;

import org.folio.consortia.domain.dto.UserCollection;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;

@HttpExchange("users")
public interface UsersClient {

  @GetExchange(accept = MediaType.APPLICATION_JSON_VALUE)
  UserCollection getUserCollection(@RequestParam String query, @RequestParam int offset, @RequestParam int limit);

  @GetExchange
  UserCollection getUsersByQuery(@RequestParam("query") String query);
}
