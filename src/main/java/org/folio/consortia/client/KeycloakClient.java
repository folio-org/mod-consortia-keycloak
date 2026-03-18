package org.folio.consortia.client;

import static org.springframework.http.HttpHeaders.AUTHORIZATION;
import static org.springframework.http.MediaType.APPLICATION_FORM_URLENCODED_VALUE;
import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.node.ObjectNode;

import java.util.List;
import java.util.Map;

import org.folio.consortia.domain.dto.KeycloakClientCredentials;
import org.folio.consortia.domain.dto.KeycloakIdentityProvider;
import org.folio.consortia.domain.dto.KeycloakTokenResponse;
import org.folio.consortia.domain.dto.RealmExecutions;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.service.annotation.DeleteExchange;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;
import org.springframework.web.service.annotation.PostExchange;
import org.springframework.web.service.annotation.PutExchange;

@HttpExchange
public interface KeycloakClient {

  /**
   * Login with backend admin cli credentials to get authorization token in master realm
   *
   * @param loginRequest containing following properties: client_id, client_secret, grant_type
   * @return KeycloakTokenResponse
   */
  @PostExchange(value = "/realms/master/protocol/openid-connect/token", contentType = APPLICATION_FORM_URLENCODED_VALUE)
  KeycloakTokenResponse login(@RequestBody MultiValueMap<String, ?> loginRequest);

  /**
   * Get client credentials for a given client id in a realm
   *
   * @param realm    the realm to get client credentials from
   * @param clientId the login client id
   * @param token    authorization token
   * @return list of KeycloakClientCredentials (should contain only one element)
   */
  @GetExchange(value = "/admin/realms/{realm}/clients", accept = APPLICATION_JSON_VALUE)
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
  @GetExchange("/admin/realms/{realm}/identity-provider/instances/{alias}")
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
  @PostExchange("/admin/realms/{realm}/identity-provider/instances")
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
  @DeleteExchange("/admin/realms/{realm}/identity-provider/instances/{alias}")
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
  @GetExchange(value = "/admin/realms/{tenant}")
  ObjectNode getRealm(@PathVariable("tenant") String tenant,
                      @RequestHeader(AUTHORIZATION) String token);

  /**
   * Updates the realm configuration for the specified tenant.
   *
   * @param tenant the tenant identifier
   * @param realm  the updated realm configuration as a JsonNode
   * @param token  the authorization token
   */
  @PutExchange(value = "/admin/realms/{tenant}")
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
  @PostExchange(value = "/admin/realms/{tenant}/authentication/flows/browser/copy")
  void copyBrowserFlow(@PathVariable("tenant") String tenant,
                       @RequestBody Map<String, String> copyRequest,
                       @RequestHeader(AUTHORIZATION) String token);

  /**
   * Retrieves the executions for the specified authentication flow in the specified tenant.
   *
   * @param tenant   the tenant identifier
   * @param flowName the name of the authentication flow
   * @param token    the authorization token
   * @return the list of executions for the specified flow
   */
  @GetExchange(value = "/admin/realms/{tenant}/authentication/flows/{flowName}/executions")
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
  @PostExchange(value = "/admin/realms/{tenant}/authentication/flows/{flowName}/executions/execution")
  void executeBrowserFlow(@PathVariable("tenant") String tenant,
                          @PathVariable("flowName") String flowName,
                          @RequestBody Map<String, String> executionRequest,
                          @RequestHeader(AUTHORIZATION) String token);

  /**
   * Raises the priority of the specified execution in the specified tenant.
   *
   * @param tenant      the tenant identifier
   * @param executionId the identifier of the execution to raise priority
   * @param token       the authorization token
   */
  @PostExchange(value = "/admin/realms/{tenant}/authentication/executions/{executionId}/raise-priority")
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
  @DeleteExchange(value = "/admin/realms/{tenant}/authentication/executions/{executionId}")
  void deleteExecution(@PathVariable("tenant") String tenant,
                       @PathVariable("executionId") String executionId,
                       @RequestHeader(AUTHORIZATION) String token);
}
