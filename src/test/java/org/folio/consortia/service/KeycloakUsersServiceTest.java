package org.folio.consortia.service;

import static org.folio.consortia.support.EntityUtils.CENTRAL_TENANT_ID;
import static org.folio.consortia.support.EntityUtils.TENANT_ID;
import static org.folio.consortia.support.EntityUtils.getFolioExecutionContext;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.folio.consortia.client.UsersKeycloakClient;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.dto.UserMigrationRequest;
import org.folio.consortia.service.impl.KeycloakUsersServiceImpl;
import org.folio.consortia.support.CopilotGenerated;
import org.folio.spring.FolioExecutionContext;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
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
  @Spy
  private FolioExecutionContext folioExecutionContext = getFolioExecutionContext();

  @InjectMocks
  private KeycloakUsersServiceImpl keycloakUsersService;

  @Test
  void migrateUsers_migratesUsersSuccessfully() {
    User user = new User();
    user.setId("userId");
    when(userService.getPrimaryUsersToLink()).thenReturn(List.of(user));

    keycloakUsersService.migrateUsers(TENANT_ID, CENTRAL_TENANT_ID);

    verify(usersKeycloakClient).migrateUsers(any(UserMigrationRequest.class));
  }

  @Test
  void migrateUsers_doesNothingIfNoUsersToMigrate() {
    when(userService.getPrimaryUsersToLink()).thenReturn(Collections.emptyList());

    keycloakUsersService.migrateUsers(TENANT_ID, CENTRAL_TENANT_ID);

    verify(usersKeycloakClient, never()).migrateUsers(any(UserMigrationRequest.class));
  }

}
