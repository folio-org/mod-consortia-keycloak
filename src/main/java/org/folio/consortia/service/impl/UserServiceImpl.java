package org.folio.consortia.service.impl;

import static org.folio.consortia.utils.TenantContextUtils.prepareContextForTenant;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import org.folio.consortia.client.UsersClient;
import org.folio.consortia.client.UsersKeycloakClient;
import org.folio.consortia.domain.dto.Personal;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.dto.UserType;
import org.folio.consortia.exception.ConsortiumClientException;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.service.UserService;
import org.folio.consortia.utils.HelperUtils;
import org.folio.consortia.utils.TenantContextUtils;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.scope.FolioExecutionContextSetter;
import org.springframework.stereotype.Service;

import feign.FeignException;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@Log4j2
@RequiredArgsConstructor
public class UserServiceImpl implements UserService {
  private static final String USER_ID = "userId";

  private final UsersKeycloakClient usersKeycloakClient;
  private final UsersClient usersClient;
  private final FolioExecutionContext folioExecutionContext;
  private final FolioModuleMetadata folioModuleMetadata;
  private static final Integer RANDOM_STRING_COUNT = 5;
  private static final String ORIGINAL_TENANT_ID_REF_ID = "originaltenantid";

  @Override
  public User createUser(User user) {
    log.info("Creating user with id {}.", user.getId());
    usersKeycloakClient.saveUser(user);
    return user;
  }

  public void updateUser(User user) {
    log.info("Updating User '{}'.", user.getId());
    usersKeycloakClient.updateUser(user.getId(), user);
  }

  @Override
  public User getById(UUID userId) {
    try {
      log.info("Getting user by userId {}.", userId);
      return usersKeycloakClient.getUsersByUserId(String.valueOf(userId));
    } catch (FeignException.NotFound e) {
      log.info("User with userId {} does not exist in schema, going to use new one", userId);
      return new User();
    } catch (FeignException.Forbidden e) {
      throw new ConsortiumClientException(e);
    } catch (FeignException e) {
      throw new IllegalStateException(e);
    }
  }

  @Override
  public Optional<User> getByUsername(String username) {
    return usersClient.getUsersByQuery("username==" + username)
      .getUsers()
      .stream()
      .findFirst();
  }

  @Override
  public List<User> getUsersByQuery(String query, int offset, int limit) {
    return usersClient.getUserCollection(query, offset, limit).getUsers();
  }

  @Override
  public void deleteById(String userId) {
    usersKeycloakClient.deleteUser(userId);
  }

  public User prepareShadowUser(UUID userId, String tenantId) {
    try (var context = new FolioExecutionContextSetter(
      prepareContextForTenant(tenantId, folioModuleMetadata, folioExecutionContext))) {
      log.info("prepareShadowUser:: Try to get user of tenant={} ", folioExecutionContext.getTenantId());
      User user = new User();
      User userOptional = usersKeycloakClient.getUsersByUserId(userId.toString());

      if (Objects.nonNull(userOptional.getId())) {
        user.setId(userId.toString());
        user.setUsername(String.format("%s_%s", userOptional.getUsername(), HelperUtils.randomString(RANDOM_STRING_COUNT)));
        user.setType(UserType.SHADOW.getName());
        user.setActive(true);
        if (Objects.nonNull(userOptional.getPersonal())) {
          // these firstname, lastname fields needed to correctly build UI metadata objects
          user.setPersonal(new Personal()
            .firstName(userOptional.getPersonal().getFirstName())
            .lastName(userOptional.getPersonal().getLastName()));
        }
        user.setCustomFields(Map.of(ORIGINAL_TENANT_ID_REF_ID, tenantId));
      } else {
        log.warn("Could not find real user with id: {} in his home tenant: {}", userId.toString(), tenantId);
        throw new ResourceNotFoundException(USER_ID, userId.toString());
      }
      return user;
    }
  }
}
