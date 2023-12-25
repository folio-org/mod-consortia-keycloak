package org.folio.consortia.service.impl;

import static org.folio.common.utils.CollectionUtils.mapItems;
import static org.folio.common.utils.PaginationUtils.loadInBatches;

import org.folio.common.domain.model.error.ErrorResponse;
import org.folio.common.utils.CqlQuery;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.io.Resources;
import feign.FeignException;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.folio.consortia.client.CapabilitiesClient;
import org.folio.consortia.client.UserCapabilitiesClient;
import org.folio.consortia.client.UserPermissionsClient;
import org.folio.consortia.domain.dto.Capabilities;
import org.folio.consortia.domain.dto.Capability;
import org.folio.consortia.domain.dto.PermissionUser;
import org.folio.consortia.domain.dto.UserCapabilitiesRequest;
import org.folio.consortia.service.PermissionUserService;
import org.springframework.stereotype.Service;

@Service
@Log4j2
@RequiredArgsConstructor
public class CapabilitiesUserService implements PermissionUserService {

  private static final String CAPABILITIES_UP_TO_DATE_ERROR_MSG =
    "Nothing to update, user-capability relations are not changed";
  private static final int CAPABILITY_BATCH_SIZE = 50;
  private static final String PERMISSION_QUERY_FIELD = "permission";

  private final CapabilitiesClient capabilitiesClient;
  private final UserCapabilitiesClient userCapabilitiesClient;
  private final UserPermissionsClient userPermissionsClient;
  private final ObjectMapper objectMapper;

  @Override
  public Optional<PermissionUser> getByUserId(String userId) {
    return Optional.of(userPermissionsClient.getPermissionsForUser(userId, false));
  }

  @Override
  public PermissionUser createWithEmptyPermissions(String userId) {
    throw new UnsupportedOperationException("User cannot be assigned with empty set of capabilities");
  }

  @Override
  public PermissionUser createWithPermissionsFromFile(String userId, String permissionsFilePath) {
    var perms = readAndValidatePermissions(permissionsFilePath);
    var permissionUser = PermissionUser.of(UUID.randomUUID().toString(), userId, perms);
    log.info("Creating permissionUser {}.", permissionUser);
    assignPermissions(permissionUser.getUserId(), perms);
    return permissionUser;
  }

  @Override
  public void addPermissions(PermissionUser permissionUser, String permissionsFilePath) {
    var permissions = readAndValidatePermissions(permissionsFilePath);
    assignPermissions(permissionUser.getUserId(), permissions);
  }

  @Override
  public void deletePermissionUser(String userId) {
    userCapabilitiesClient.deleteUserCapabilities(userId);
    log.info("deleteUserPermissions:: Deleted capabilities with userId={}", userId);
  }

  private List<String> readAndValidatePermissions(String permissionsFilePath) {
    var permissions = readPermissionsFromResource(permissionsFilePath);
    if (CollectionUtils.isEmpty(permissions)) {
      throw new IllegalStateException("No user permissions found in " + permissionsFilePath);
    }
    return permissions;
  }

  private List<String> readPermissionsFromResource(String permissionsFilePath) {
    List<String> result;
    var url = Resources.getResource(permissionsFilePath);

    try {
      result = Resources.readLines(url, StandardCharsets.UTF_8);
    } catch (IOException e) {
      log.error("Can't read user permissions from {}.", permissionsFilePath, e);
      throw new IllegalStateException("Can't read user permissions... ", e);
    }
    return result;
  }

  private void assignPermissions(String userId, List<String> permissions) {
    log.info("Resolving capabilities by permissions: {}", permissions);
    var capabilities = findCapabilitiesByPermissions(permissions);
    if (CollectionUtils.isEmpty(capabilities)) {
      log.warn("No capabilities found");
      return;
    }

    var ids = mapItems(capabilities, Capability::getId);
    log.info("Assigning resolved capabilities: {}", ids);
    assignCapabilities(userId, ids);
  }

  private void assignCapabilities(String userId, List<UUID> capabilityIds) {
    try {
      var request = new UserCapabilitiesRequest().userId(userId).capabilityIds(capabilityIds);
      userCapabilitiesClient.assignUserCapabilities(userId, request);
    } catch (FeignException e) {
      if (isNothingToUpdateError(e)) {
        log.info("User capabilities are up to date");
        return;
      }
      throw e;
    }
  }

  private boolean isNothingToUpdateError(FeignException feignException) {
    var content = feignException.contentUTF8();
    try {
      var response = objectMapper.readValue(content, ErrorResponse.class);
      if (response.getTotalRecords() == 1) {
        var error = response.getErrors().get(0);
        return CAPABILITIES_UP_TO_DATE_ERROR_MSG.equals(error.getMessage());
      }
    } catch (Exception e) {
      log.warn("Unable to parse error: '{}'", content, e);
    }
    return false;
  }

  /**
   * Queries capabilities in batches by permissions.
   *
   * @param permissions permissions
   * @return List of capabilities
   */
  private List<Capability> findCapabilitiesByPermissions(List<String> permissions) {
    return loadInBatches(permissions, permissionsBatch ->
      queryCapabilities(permissionsBatch).getCapabilities(), CAPABILITY_BATCH_SIZE);
  }

  private Capabilities queryCapabilities(List<String> permissions) {
    var query = CqlQuery.exactMatchAny(PERMISSION_QUERY_FIELD, permissions).toString();
    return capabilitiesClient.queryCapabilities(query, CAPABILITY_BATCH_SIZE, 0);
  }
}
