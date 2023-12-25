package org.folio.consortia.client;

import org.folio.consortia.domain.dto.UserCollection;
import org.folio.spring.config.FeignClientConfiguration;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "consortia-users-client", url = "users" , configuration = FeignClientConfiguration.class)
public interface UsersClient {

  @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
  UserCollection getUserCollection(@RequestParam String query, @RequestParam int offset, @RequestParam int limit);

  @GetMapping
  UserCollection getUsersByQuery(@RequestParam("query") String query);
}
