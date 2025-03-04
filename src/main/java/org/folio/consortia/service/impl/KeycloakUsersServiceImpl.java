package org.folio.consortia.service.impl;

import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.consortia.client.UsersKeycloakClient;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.dto.UserMigrationRequest;
import org.folio.consortia.service.KeycloakUsersService;
import org.folio.consortia.service.UserService;
import org.folio.consortia.utils.TenantContextUtils;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.scope.FolioExecutionContextSetter;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class KeycloakUsersServiceImpl implements KeycloakUsersService {

  private final UserService userService;
  private final UsersKeycloakClient usersKeycloakClient;
  private final FolioExecutionContext folioExecutionContext;

  @Override
  public void migrateUsers(String tenantId, String centralTenantId) {
    log.info("migrateUsers:: Migrating users for central tenant: '{}'", centralTenantId);
    var users = getMemberTenantOriginalUsers(tenantId);
    if (CollectionUtils.isEmpty(users)) {
      log.info("migrateUsers:: No users to migrate for central tenant: '{}'", centralTenantId);
      return;
    }
    log.info("migrateUsers:: Found '{}' users to migrate for central tenant: '{}'", users.size(), centralTenantId);
    var userMigrationRequest = new UserMigrationRequest()
      .userIds(users.stream().map(User::getId).collect(Collectors.toSet()))
      .centralTenantId(centralTenantId);
    usersKeycloakClient.migrateUsers(userMigrationRequest);
  }

  private List<User> getMemberTenantOriginalUsers(String tenantId) {
    var memberTenantContext = TenantContextUtils.prepareContextForTenant(tenantId, folioExecutionContext.getFolioModuleMetadata(), folioExecutionContext);
    try (var ignored = new FolioExecutionContextSetter(memberTenantContext)) {
      return userService.getPrimaryUsersToLink();
    }
  }

}
