package org.folio.consortia.client;

import static org.springframework.http.HttpHeaders.AUTHORIZATION;
import static org.springframework.http.MediaType.APPLICATION_FORM_URLENCODED_VALUE;
import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.util.List;
import java.util.Map;

import org.folio.consortia.config.keycloak.KeycloakFeignClientConfig;
import org.folio.consortia.domain.dto.KeycloakClientCredentials;
import org.folio.consortia.domain.dto.KeycloakIdentityProvider;
import org.folio.consortia.domain.dto.KeycloakTokenResponse;
import org.folio.consortia.domain.dto.RealmExecutions;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "keycloak",
  url = "#{keycloakProperties.url}",
  configuration = KeycloakFeignClientConfig.class)
public interface KeycloakClient {

  /**
   * Login with backend admin cli credentials to get authorization token in master realm
   *
   * @param loginRequest containing following properties: client_id, client_secret, grant_type
   * @return KeycloakTokenResponse
   */
  @PostMapping(value = "/realms/master/protocol/openid-connect/token", consumes = APPLICATION_FORM_URLENCODED_VALUE)
  KeycloakTokenResponse login(@RequestBody Map<String, ?> loginRequest);

  /**
   * Get client credentials for a given client id in a realm
   *
   * @param realm    the realm to get client credentials from
   * @param clientId the login client id
   * @param token    authorization token
   * @return list of KeycloakClientCredentials (should contain only one element)
   */
  @GetMapping(value = "/admin/realms/{realm}/clients", produces = APPLICATION_JSON_VALUE)
  List<KeycloakClientCredentials> getClientCredentials(@PathVariable("realm") String realm,
                                                       @RequestParam("clientId") String clientId,
                                                       @RequestHeader(AUTHORIZATION) String token);

  /**
   * Get identity provider by alias in a realm
   *
   * @param realm realm to get identity provider from
   * @param alias alias of the identity provider
   * @param token authorization token
   * @return KeycloakIdentityProvider
   */
  @GetMapping("/admin/realms/{realm}/identity-provider/instances/{alias}")
  KeycloakIdentityProvider getIdentityProvider(@PathVariable("realm") String realm,
                                               @PathVariable("alias") String alias,
                                               @RequestHeader(AUTHORIZATION) String token);

  /**
   * Create identity provider in a realm
   *
   * @param realm            realm to create identity provider in
   * @param identityProvider identity provider to create
   * @param token            authorization token
   */
  @PostMapping("/admin/realms/{realm}/identity-provider/instances")
  void createIdentityProvider(@PathVariable("realm") String realm,
                              @RequestBody KeycloakIdentityProvider identityProvider,
                              @RequestHeader(AUTHORIZATION) String token);

  /**
   * Delete identity provider by alias in a realm
   *
   * @param realm realm to delete identity provider from
   * @param alias alias of the identity provider
   * @param token authorization token
   */
  @DeleteMapping("/admin/realms/{realm}/identity-provider/instances/{alias}")
  void deleteIdentityProvider(@PathVariable("realm") String realm,
                              @PathVariable("alias") String alias,
                              @RequestHeader(AUTHORIZATION) String token);

  /**
   * Retrieves the realm configuration for the specified tenant.
   *
   * @param tenant the tenant identifier
   * @param token  the authorization token
   * @return the realm configuration as an ObjectNode
   */
  @GetMapping(value = "admin/realms/{tenant}")
  ObjectNode getRealm(@PathVariable("tenant") String tenant,
                      @RequestHeader(AUTHORIZATION) String token);

  /**
   * Updates the realm configuration for the specified tenant.
   *
   * @param tenant the tenant identifier
   * @param realm  the updated realm configuration as a JsonNode
   * @param token  the authorization token
   */
  @PutMapping(value = "/admin/realms/{tenant}")
  void updateRealm(@PathVariable("tenant") String tenant,
                   @RequestBody JsonNode realm,
                   @RequestHeader(AUTHORIZATION) String token);

  /**
   * Duplicates the built-in browser authentication flow for the specified tenant.
   *
   * @param tenant      the tenant identifier
   * @param copyRequest the request containing the new flow name
   * @param token       the authorization token
   */
  @PostMapping(value = "/admin/realms/{tenant}/authentication/flows/browser/copy")
  void copyBrowserFlow(@PathVariable("tenant") String tenant,
                       @RequestBody Map<String, ?> copyRequest,
                       @RequestHeader(AUTHORIZATION) String token);

  /**
   * Retrieves the executions for the specified authentication flow in the specified tenant.
   *
   * @param tenant   the tenant identifier
   * @param flowName the name of the authentication flow
   * @param token    the authorization token
   * @return the list of executions for the specified flow
   */
  @GetMapping(value = "/admin/realms/{tenant}/authentication/flows/{flowName}/executions")
  List<RealmExecutions> getExecutions(@PathVariable("tenant") String tenant,
                                      @PathVariable("flowName") String flowName,
                                      @RequestHeader(AUTHORIZATION) String token);

  /**
   * Adds an execution to the specified authentication flow in the specified tenant.
   *
   * @param tenant           the tenant identifier
   * @param flowName         the name of the authentication flow
   * @param executionRequest the request containing the execution details
   * @param token            the authorization token
   */
  @PostMapping(value = "/admin/realms/{tenant}/authentication/flows/{flowName}/executions/execution")
  void executeBrowserFlow(@PathVariable("tenant") String tenant,
                          @PathVariable("flowName") String flowName,
                          @RequestBody Map<String, ?> executionRequest,
                          @RequestHeader(AUTHORIZATION) String token);

  /**
   * Raises the priority of the specified execution in the specified tenant.
   *
   * @param tenant      the tenant identifier
   * @param executionId the identifier of the execution to raise priority
   * @param token       the authorization token
   */
  @PostMapping(value = "/admin/realms/{tenant}/authentication/executions/{executionId}/raise-priority")
  void raisePriority(@PathVariable("tenant") String tenant,
                     @PathVariable("executionId") String executionId,
                     @RequestHeader(AUTHORIZATION) String token);

  /**
   * Deletes the specified execution from the specified tenant.
   *
   * @param tenant      the tenant identifier
   * @param executionId the identifier of the execution to delete
   * @param token       the authorization token
   */
  @DeleteMapping(value = "/admin/realms/{tenant}/authentication/executions/{executionId}")
  void deleteExecution(@PathVariable("tenant") String tenant,
                       @PathVariable("executionId") String executionId,
                       @RequestHeader(AUTHORIZATION) String token);
}
