package org.folio.consortia.client;

import static org.springframework.http.HttpHeaders.AUTHORIZATION;
import static org.springframework.http.MediaType.APPLICATION_FORM_URLENCODED_VALUE;
import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE;

import java.util.List;
import java.util.Map;

import org.folio.consortia.config.keycloak.KeycloakFeignClientConfig;
import org.folio.consortia.domain.dto.KeycloakClientCredentials;
import org.folio.consortia.domain.dto.KeycloakIdentityProvider;
import org.folio.consortia.domain.dto.KeycloakTokenResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "keycloak",
  url = "#{keycloakProperties.url}",
  configuration = KeycloakFeignClientConfig.class)
public interface KeycloakClient {

  @PostMapping(value = "/realms/master/protocol/openid-connect/token", consumes = APPLICATION_FORM_URLENCODED_VALUE)
  KeycloakTokenResponse login(@RequestBody Map<String, ?> loginRequest);

  @GetMapping(value = "/admin/realms/{realm}/clients", produces = APPLICATION_JSON_VALUE)
  List<KeycloakClientCredentials> getClientCredentials(@PathVariable("realm") String realm,
                                                       @RequestParam("clientId") String clientId,
                                                       @RequestHeader(AUTHORIZATION) String token);

  @GetMapping("/admin/realms/{realm}/identity-provider/instances/{alias}")
  KeycloakIdentityProvider getIdentityProvider(@PathVariable("realm") String realm,
                                               @PathVariable("alias") String alias,
                                               @RequestHeader(AUTHORIZATION) String token);

  @DeleteMapping("/admin/realms/{realm}/identity-provider/instances/{alias}")
  void deleteIdentityProvider(@PathVariable("realm") String realm,
                              @PathVariable("alias") String alias,
                              @RequestHeader(AUTHORIZATION) String token);

  @PostMapping("/admin/realms/{realm}/identity-provider/instances")
  void createIdentityProvider(@PathVariable("realm") String realm,
                              @RequestBody KeycloakIdentityProvider identityProvider,
                              @RequestHeader(AUTHORIZATION) String token);

}
