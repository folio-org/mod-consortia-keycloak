package org.folio.consortia.client;

import static org.springframework.http.MediaType.APPLICATION_FORM_URLENCODED_VALUE;

import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.databind.JsonNode;

import org.folio.consortia.config.keycloak.KeycloakFeignClientConfig;
import org.folio.consortia.domain.dto.TokenResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;

@FeignClient(name = "keycloak",
  url = "#{keycloakProperties.url}",
  configuration = KeycloakFeignClientConfig.class)
public interface KeycloakClient {

  @PostMapping(value = "/realms/master/protocol/openid-connect/token", consumes = APPLICATION_FORM_URLENCODED_VALUE)
  TokenResponse login(@RequestBody Map<String, ?> loginRequest);

  @PostMapping(value = "/realms/{tenant}/protocol/openid-connect/token", consumes = APPLICATION_FORM_URLENCODED_VALUE)
  TokenResponse login(@RequestBody Map<String, ?> loginRequest, @PathVariable("tenant") String tenant);

  @PostMapping(value = "/admin/realms/{tenant}/authentication/flows/browser/copy")
  void copyBrowserFlow(@PathVariable("tenant") String tenant, @RequestBody Map<String, ?> copyRequest);

  @GetMapping(value = "/admin/realms/{tenant}/authentication/flows/{flowName}/executions")
  List<JsonNode> getExecutions(@PathVariable("tenant") String tenant, @PathVariable("flowName") String flowName);

  @PostMapping(value = "/admin/realms/{tenant}/authentication/flows/{flowName}/executions/execution")
  void executeBrowserFlow(@PathVariable("tenant") String tenant, @PathVariable("flowName") String flowName, @RequestBody Map<String, ?> executionRequest);

  @PostMapping(value = "/admin/realms/{tenant}/authentication/flows/{flowName}/executions/{executionId}/raise-priority")
  void raisePriority(@PathVariable("tenant") String tenant, @PathVariable("flowName") String flowName, @PathVariable("executionId") String executionId);

  @DeleteMapping(value = "/admin/realms/{tenant}/authentication/flows/{flowName}/executions/{executionId}")
  void deleteExecution(@PathVariable("tenant") String tenant, @PathVariable("flowName") String flowName, @PathVariable("executionId") String executionId);

  @PutMapping(value = "/admin/realms/{tenant}/authentication/flows/{flowName}")
  void updateFlow(@PathVariable("tenant") String tenant, @PathVariable("flowName") String flowName, @RequestBody Map<String, ?> flowRequest);
}
