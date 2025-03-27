package org.folio.consortia.service.impl;

import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.folio.consortia.client.UsersKeycloakClient;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.dto.UsersIdpLinkOperationRequest;
import org.folio.consortia.service.KeycloakUsersService;
import org.folio.consortia.service.UserService;
import org.springframework.stereotype.Service;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class KeycloakUsersServiceImpl implements KeycloakUsersService {

  private final UserService userService;
  private final UsersKeycloakClient usersKeycloakClient;

  @Override
  public void createUsersIdpLinks(String centralTenantId, String memberTenantId) {
    log.info("createUsersIdpLinks:: Creating IDP links for users from original member tenant: '{}' in central tenant: '{}'", memberTenantId, centralTenantId);
    applyUsersIdpLinkOperation(centralTenantId, memberTenantId, usersKeycloakClient::createUsersIdpLinks);
  }

  @Override
  public void removeUsersIdpLinks(String centralTenantId, String memberTenantId) {
    log.info("removeUsersIdpLinks:: Removing IDP links for users from original member tenant: '{}' in central tenant: '{}'", memberTenantId, centralTenantId);
    applyUsersIdpLinkOperation(centralTenantId, memberTenantId, usersKeycloakClient::deleteUsersIdpLinks);
  }

  @Override
  public void recreateUserIdpLink(String centralTenantId, String userId) {
    log.info("recreateUserIdpLink:: Recreating IDP link for user: '{}' in central tenant: '{}'", userId, centralTenantId);
    usersKeycloakClient.createUsersIdpLinks(new UsersIdpLinkOperationRequest(Set.of(userId), centralTenantId));
  }

  private void applyUsersIdpLinkOperation(String centralTenantId, String memberTenantId, Consumer<UsersIdpLinkOperationRequest> linkOperation) {
    var users = userService.getPrimaryUsersToLink(memberTenantId);
    if (CollectionUtils.isEmpty(users)) {
      log.info("applyUsersIdpLinkOperation:: No users for link operation between member tenant: '{}' and central tenant: '{}'", memberTenantId, centralTenantId);
      return;
    }
    log.info("applyUsersIdpLinkOperation:: Found '{}' users for link operation between member tenant: '{}' and central tenant: '{}'", users.size(), memberTenantId, centralTenantId);
    var usersIdpLinkOperationRequest = new UsersIdpLinkOperationRequest()
      .userIds(users.stream().map(User::getId).collect(Collectors.toSet()))
      .centralTenantId(centralTenantId);
    linkOperation.accept(usersIdpLinkOperationRequest);
  }

}
