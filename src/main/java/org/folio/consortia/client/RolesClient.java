package org.folio.consortia.client;

import org.folio.consortia.domain.dto.Roles;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "roles")
public interface RolesClient {

  @GetMapping
  Roles getRolesByQuery(@RequestParam("query") String query);

}
