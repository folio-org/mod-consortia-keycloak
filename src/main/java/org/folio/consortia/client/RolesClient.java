package org.folio.consortia.client;

import org.folio.consortia.domain.dto.Roles;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;

@HttpExchange("roles")
public interface RolesClient {

  @GetExchange
  Roles getRolesByQuery(@RequestParam("query") String query);

}
