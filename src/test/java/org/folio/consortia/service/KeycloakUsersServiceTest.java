package org.folio.consortia.service;

import static org.folio.consortia.support.EntityUtils.CENTRAL_TENANT_ID;
import static org.folio.consortia.support.EntityUtils.TENANT_ID;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.folio.consortia.client.UsersKeycloakClient;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.dto.UsersIdpLinkOperationRequest;
import org.folio.consortia.service.impl.KeycloakUsersServiceImpl;
import org.folio.consortia.support.CopilotGenerated;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;
import java.util.Collections;

@ExtendWith(MockitoExtension.class)
@CopilotGenerated
class KeycloakUsersServiceTest {

  @Mock
  private UserService userService;
  @Mock
  private UsersKeycloakClient usersKeycloakClient;

  @InjectMocks
  private KeycloakUsersServiceImpl keycloakUsersService;

  @Test
  void testCreateUsersIdpLinks() {
    User user = new User();
    user.setId("userId");
    when(userService.getPrimaryUsersToLink(TENANT_ID)).thenReturn(List.of(user));

    keycloakUsersService.createUsersIdpLinks(CENTRAL_TENANT_ID, TENANT_ID);

    verify(usersKeycloakClient).createUsersIdpLinks(any(UsersIdpLinkOperationRequest.class));
  }

  @Test
  void testCreateUsersIdpLinksNoUsersToLink() {
    when(userService.getPrimaryUsersToLink(TENANT_ID)).thenReturn(Collections.emptyList());

    keycloakUsersService.createUsersIdpLinks(CENTRAL_TENANT_ID, TENANT_ID);

    verify(usersKeycloakClient, never()).createUsersIdpLinks(any(UsersIdpLinkOperationRequest.class));
  }

  @Test
  void testRemoveUsersIdpLinks() {
    User user = new User();
    user.setId("userId");
    when(userService.getPrimaryUsersToLink(TENANT_ID)).thenReturn(List.of(user));

    keycloakUsersService.removeUsersIdpLinks(CENTRAL_TENANT_ID, TENANT_ID);

    verify(usersKeycloakClient).deleteUsersIdpLinks(any(UsersIdpLinkOperationRequest.class));
  }

  @Test
  void testRemoveUsersIdpLinksNoUsersToUnlink() {
    when(userService.getPrimaryUsersToLink(TENANT_ID)).thenReturn(Collections.emptyList());

    keycloakUsersService.removeUsersIdpLinks(CENTRAL_TENANT_ID, TENANT_ID);

    verify(usersKeycloakClient, never()).deleteUsersIdpLinks(any(UsersIdpLinkOperationRequest.class));
  }

}
