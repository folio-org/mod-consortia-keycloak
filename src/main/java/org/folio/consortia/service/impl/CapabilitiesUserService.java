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
import org.folio.consortia.client.CapabilitySetsClient;
import org.folio.consortia.client.UserCapabilitiesClient;
import org.folio.consortia.client.UserCapabilitySetsClient;
import org.folio.consortia.client.UserPermissionsClient;
import org.folio.consortia.domain.dto.CapabilitySet;
import org.folio.consortia.domain.dto.CapabilitySets;
import org.folio.consortia.domain.dto.PermissionUser;
import org.folio.consortia.domain.dto.UserCapabilitySetsRequest;
import org.folio.consortia.service.PermissionUserService;
import org.springframework.stereotype.Service;

@Service
@Log4j2
@RequiredArgsConstructor
public class CapabilitiesUserService implements PermissionUserService {

  private static final String CAPABILITIES_UP_TO_DATE_ERROR_MSG =
    "Nothing to update, user-capability relations are not changed";
  private static final int CAPABILITY_SET_BATCH_SIZE = 50;
  private static final String PERMISSION_QUERY_FIELD = "permission";

  private final CapabilitySetsClient capabilitySetsClient;
  private final UserCapabilitiesClient userCapabilitiesClient;
  private final UserCapabilitySetsClient userCapabilitySetsClient;
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
  public PermissionUser createWithPermissionSetsFromFile(String userId, String permissionSetsFilePath) {
    var perms = readAndValidatePermissions(permissionSetsFilePath);
    var permissionUser = PermissionUser.of(UUID.randomUUID().toString(), userId, perms);
    log.info("Creating permissionUser {}.", permissionUser);
    assignPermissionSets(permissionUser.getUserId(), perms);
    return permissionUser;
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

  private void assignPermissionSets(String userId, List<String> permissionSets) {
    log.info("Resolving capabilities by permission sets: {}", permissionSets);
    var capabilitySets = findCapabilitySetsByPermissionSets(permissionSets);
    if (CollectionUtils.isEmpty(capabilitySets)) {
      log.warn("No capability sets found");
      return;
    }

    var ids = mapItems(capabilitySets, CapabilitySet::getId);
    log.info("Assigning resolved capabilities, ids: {}, names: {}", ids, mapItems(capabilitySets, CapabilitySet::getName));
    assignCapabilitySets(userId, ids);
  }

  private void assignCapabilitySets(String userId, List<UUID> capabilitySetIds) {
    try {
      var request = new UserCapabilitySetsRequest().userId(userId).capabilitySetIds(capabilitySetIds);
      userCapabilitySetsClient.assignUserCapabilitySets(userId, request);
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
   * Queries capability sets in batches by permission sets.
   *
   * @param permissionSets permission sets
   * @return List of capability sets
   */
  private List<CapabilitySet> findCapabilitySetsByPermissionSets(List<String> permissionSets) {
    return loadInBatches(permissionSets, permissionsBatch ->
      queryCapabilitySets(permissionsBatch).getCapabilitySets(), CAPABILITY_SET_BATCH_SIZE);
  }

  private CapabilitySets queryCapabilitySets(List<String> permissions) {
    var query = CqlQuery.exactMatchAny(PERMISSION_QUERY_FIELD, permissions).toString();
    return capabilitySetsClient.queryCapabilitySets(query, CAPABILITY_SET_BATCH_SIZE, 0);
  }
}
