package org.folio.consortia.service.impl;

import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.consortia.client.UsersKeycloakClient;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.dto.UserIdpLinkingRequest;
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
  public void createUsersIdpLinks(String centralTenantId, String memberTenantId) {
    log.info("createUsersIdpLinks:: Creating users IDP links created in member tenant: '{}' in central tenant: '{}'", memberTenantId, centralTenantId);
    var users = getMemberTenantOriginalUsers(memberTenantId);
    if (CollectionUtils.isEmpty(users)) {
      log.info("createUsersIdpLinks:: No users to create links from member tenant: '{}' to central tenant: '{}'", memberTenantId, centralTenantId);
      return;
    }
    log.info("createUsersIdpLinks:: Found '{}' users to create links from member tenant: '{}' to central tenant: '{}'", users.size(), memberTenantId, centralTenantId);
    var userIdpLinkingRequest = new UserIdpLinkingRequest()
      .userIds(users.stream().map(User::getId).collect(Collectors.toSet()))
      .centralTenantId(centralTenantId);
    usersKeycloakClient.createUsersIdpLinks(userIdpLinkingRequest);
  }

  private List<User> getMemberTenantOriginalUsers(String tenantId) {
    var memberTenantContext = TenantContextUtils.prepareContextForTenant(tenantId, folioExecutionContext.getFolioModuleMetadata(), folioExecutionContext);
    try (var ignored = new FolioExecutionContextSetter(memberTenantContext)) {
      return userService.getPrimaryUsersToLink();
    }
  }

}
