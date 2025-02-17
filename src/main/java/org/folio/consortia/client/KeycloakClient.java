package org.folio.consortia.client;

import org.folio.consortia.config.keycloak.KeycloakFeignClientConfig;
import org.folio.consortia.domain.dto.KeycloakIdentityProvider;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

@FeignClient(name = "keycloak",
  url = "#{keycloakProperties.url}",
  configuration = KeycloakFeignClientConfig.class)
public interface KeycloakClient {

  @GetMapping("/admin/realms/{realm}/identity-provider/instances/{alias}")
  KeycloakIdentityProvider getIdentityProvider(@PathVariable("realm") String realm, @PathVariable("alias") String alias);

  @DeleteMapping("/admin/realms/{realm}/identity-provider/instances/{alias}")
  void deleteIdentityProvider(@PathVariable("realm") String realm, @PathVariable("alias") String alias);

  @PostMapping("/admin/realms/{realm}/identity-provider/instances")
  void createIdentityProvider(@PathVariable("realm") String realm, @RequestBody KeycloakIdentityProvider identityProvider);

}
