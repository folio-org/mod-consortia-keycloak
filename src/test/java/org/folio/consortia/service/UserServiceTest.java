package org.folio.consortia.service;

import static org.folio.consortia.utils.EntityUtils.createUserEntity;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import org.folio.consortia.client.UsersKeycloakClient;
import org.folio.consortia.domain.dto.UserType;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.service.impl.UserServiceImpl;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.folio.consortia.domain.dto.User;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.integration.XOkapiHeaders;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
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
    Assertions.assertEquals(user, createdUser);
  }

  @Test
  void shouldUpdateUser() {
    User user = createUserEntity(false);
    Mockito.doNothing().when(usersKeycloakClient).updateUser(user.getId(), user);
    Assertions.assertDoesNotThrow(() -> userService.updateUser(user));
  }

  @Test
  void shouldDeleteUser() {
    User user = createUserEntity(false);
    Mockito.doNothing().when(usersKeycloakClient).deleteUser(user.getId());
    Assertions.assertDoesNotThrow(() -> userService.deleteById(user.getId()));
  }

  @Test
  void shouldThrowNotFoundWhilePrepareShadowUser() {
    when(folioExecutionContext.getTenantId()).thenReturn("diku");
    Map<String, Collection<String>> okapiHeaders = new HashMap<>();
    okapiHeaders.put(XOkapiHeaders.TENANT, List.of("diku"));
    when(folioExecutionContext.getOkapiHeaders()).thenReturn(okapiHeaders);
    Mockito.when(usersKeycloakClient.getUsersByUserId(any())).thenReturn(new User());
    Assertions.assertThrows(ResourceNotFoundException.class, () -> userService.prepareShadowUser(UUID.randomUUID(), ""));
  }

  @Test
  void shouldPrepareShadowUser() {
    when(folioExecutionContext.getTenantId()).thenReturn("diku");
    when(folioExecutionContext.getFolioModuleMetadata()).thenReturn(folioModuleMetadata);
    Map<String, Collection<String>> okapiHeaders = new HashMap<>();
    okapiHeaders.put(XOkapiHeaders.TENANT, List.of("diku"));
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
