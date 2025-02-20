package org.folio.consortia.client;

import static org.springframework.http.HttpHeaders.AUTHORIZATION;
import static org.springframework.http.MediaType.APPLICATION_FORM_URLENCODED_VALUE;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.util.List;
import java.util.Map;
import org.folio.consortia.config.keycloak.KeycloakFeignClientConfig;
import org.folio.consortia.domain.dto.RealmExecutions;
import org.folio.consortia.domain.dto.TokenResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;

@FeignClient(name = "keycloak", url = "#{keycloakProperties.url}",
  configuration = KeycloakFeignClientConfig.class)
public interface KeycloakClient {

  @PostMapping(value = "/realms/master/protocol/openid-connect/token", consumes = APPLICATION_FORM_URLENCODED_VALUE)
  TokenResponse login(@RequestBody Map<String, ?> loginRequest);

  @PostMapping(value = "/realms/{tenant}/protocol/openid-connect/token", consumes = APPLICATION_FORM_URLENCODED_VALUE)
  TokenResponse login(@RequestBody Map<String, ?> loginRequest, @PathVariable("tenant") String tenant);

  @GetMapping(value = "admin/realms/{tenant}")
  ObjectNode getRealm(@PathVariable("tenant") String tenant,
                      @RequestHeader(AUTHORIZATION) String token);

  @PutMapping(value = "/admin/realms/{tenant}")
  void updateRealm(@PathVariable("tenant") String tenant,
                   @RequestBody JsonNode realm,
                   @RequestHeader(AUTHORIZATION) String token);

  @PostMapping(value = "/admin/realms/{tenant}/authentication/flows/browser/copy")
  void copyBrowserFlow(@PathVariable("tenant") String tenant,
                       @RequestBody Map<String, ?> copyRequest,
                       @RequestHeader(AUTHORIZATION) String token);

  @GetMapping(value = "/admin/realms/{tenant}/authentication/flows/{flowName}/executions")
  List<RealmExecutions> getExecutions(@PathVariable("tenant") String tenant,
                                      @PathVariable("flowName") String flowName,
                                      @RequestHeader(AUTHORIZATION) String token);

  @PostMapping(value = "/admin/realms/{tenant}/authentication/flows/{flowName}/executions/execution")
  void executeBrowserFlow(@PathVariable("tenant") String tenant,
                          @PathVariable("flowName") String flowName,
                          @RequestBody Map<String, ?> executionRequest,
                          @RequestHeader(AUTHORIZATION) String token);

  @PostMapping(value = "/admin/realms/{tenant}/authentication/executions/{executionId}/raise-priority")
  void raisePriority(@PathVariable("tenant") String tenant,
                     @PathVariable("executionId") String executionId,
                     @RequestHeader(AUTHORIZATION) String token);

  @DeleteMapping(value = "/admin/realms/{tenant}/authentication/executions/{executionId}")
  void deleteExecution(@PathVariable("tenant") String tenant,
                       @PathVariable("executionId") String executionId,
                       @RequestHeader(AUTHORIZATION) String token);

}
