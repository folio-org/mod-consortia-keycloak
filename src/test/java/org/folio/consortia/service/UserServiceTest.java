package org.folio.consortia.service;

import static org.folio.consortia.support.EntityUtils.createOkapiHeaders;
import static org.folio.consortia.support.EntityUtils.createUserEntity;
import static org.junit.Assert.assertThrows;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import java.util.Collection;
import java.util.Map;
import java.util.UUID;
import org.folio.consortia.client.UsersKeycloakClient;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.dto.UserType;
import org.folio.consortia.service.impl.UserServiceImpl;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.batch.BatchAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
@EnableAutoConfiguration(exclude = BatchAutoConfiguration.class)
class UserServiceTest {

  @InjectMocks
  UserServiceImpl userService;
  @Mock
  UsersKeycloakClient usersKeycloakClient;
  @Mock
  FolioModuleMetadata folioModuleMetadata;
  @Mock
  FolioExecutionContext folioExecutionContext;

  @Test
  void shouldCreateUser() {
    User user = createUserEntity(false);
    Mockito.doNothing().when(usersKeycloakClient).saveUser(user);
    User createdUser = userService.createUser(user);
    assertEquals(user, createdUser);
  }

  @Test
  void shouldUpdateUser() {
    User user = createUserEntity(false);
    doNothing().when(usersKeycloakClient).updateUser(user.getId(), user);
    assertDoesNotThrow(() -> userService.updateUser(user));
  }

  @Test
  void shouldDeleteUser() {
    User user = createUserEntity(false);
    doNothing().when(usersKeycloakClient).deleteUser(user.getId());
    assertDoesNotThrow(() -> userService.deleteById(user.getId()));
  }

  @Test
  void shouldThrowNotFoundWhilePrepareShadowUser() {
    mockOkapiHeaders();
    when(usersKeycloakClient.getUsersByUserId(any())).thenReturn(new User());

    assertThrows(org.folio.consortia.exception.ResourceNotFoundException.class,
      () -> userService.prepareShadowUser(UUID.randomUUID(), ""));
  }

  @Test
  void shouldPrepareShadowUser() {
    when(usersKeycloakClient.getUsersByUserId(any())).thenReturn(createUserEntity(true));
    mockOkapiHeaders();

    User shadow = userService.prepareShadowUser(UUID.randomUUID(), "diku");

    assertEquals(UserType.SHADOW.getName(), shadow.getType());
    assertEquals("diku", shadow.getCustomFields().get("originaltenantid"));
    assertEquals(true, shadow.getActive());
    assertEquals("testFirst", shadow.getPersonal().getFirstName());
    assertEquals("testLast", shadow.getPersonal().getLastName());
    assertEquals("Test@mail.com", shadow.getPersonal().getEmail());
    assertEquals("email", shadow.getPersonal().getPreferredContactTypeId());
    assertNull(shadow.getBarcode());
  }

  @ParameterizedTest
  @EnumSource(value = UserType.class, names = {"PATRON", "DCB", "SHADOW", "SYSTEM"})
  void shouldThrowIllegalStateExceptionForNotApplicableUserType(UserType userType) {
    String tenantId = "diku";
    when(folioExecutionContext.getTenantId()).thenReturn(tenantId);
    Map<String, Collection<String>> okapiHeaders = createOkapiHeaders();
    when(folioExecutionContext.getOkapiHeaders()).thenReturn(okapiHeaders);

    UUID userId = UUID.randomUUID();
    User realUser = createUserEntity(true);
    realUser.setType(userType.getName());

    when(usersKeycloakClient.getUsersByUserId(userId.toString())).thenReturn(realUser);

    IllegalStateException exception = assertThrows(IllegalStateException.class, () -> userService.prepareShadowUser(userId, tenantId));
    assertEquals("User type is not applicable for shadow user creation", exception.getMessage());
  }

  private void mockOkapiHeaders() {
    when(folioExecutionContext.getTenantId()).thenReturn("diku");
    Map<String, Collection<String>> okapiHeaders = createOkapiHeaders();
    when(folioExecutionContext.getOkapiHeaders()).thenReturn(okapiHeaders);
    Mockito.when(usersKeycloakClient.getUsersByUserId(any())).thenReturn(createUserEntity(true));
    User user = userService.prepareShadowUser(UUID.randomUUID(), "diku");
    Assertions.assertEquals(UserType.SHADOW.getName(), user.getType());
    Assertions.assertEquals("diku", user.getCustomFields().get("originaltenantid"));
    Assertions.assertEquals(true, user.getActive());
    Assertions.assertEquals("testFirst", user.getPersonal().getFirstName());
    Assertions.assertEquals("testLast", user.getPersonal().getLastName());
  }
}
