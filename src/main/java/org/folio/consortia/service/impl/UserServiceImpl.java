package org.folio.consortia.service.impl;

import static org.folio.consortia.utils.TenantContextUtils.prepareContextForTenant;
import static org.folio.consortia.utils.TenantContextUtils.runInFolioContext;

import feign.FeignException;

import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.consortia.client.UsersClient;
import org.folio.consortia.client.UsersKeycloakClient;
import org.folio.consortia.domain.dto.Personal;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.dto.UserType;
import org.folio.consortia.exception.ConsortiumClientException;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.service.UserService;
import org.folio.consortia.utils.HelperUtils;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.scope.FolioExecutionContextSetter;
import org.springframework.stereotype.Service;

@Service
@Log4j2
@RequiredArgsConstructor
public class UserServiceImpl implements UserService {
  private static final String USER_ID = "userId";
  private static final Set<UserType> NOT_APPLICABLE_USER_TYPES = EnumSet.of(UserType.PATRON, UserType.DCB, UserType.SHADOW, UserType.SYSTEM);
  private static final String LINKABLE_USER_QUERY = "(cql.allRecords=1 NOT type=\"patron\" NOT type=\"dcb\" NOT type=\"shadow\" NOT type=\"system\")";

  private final UsersKeycloakClient usersKeycloakClient;
  private final UsersClient usersClient;
  private final FolioExecutionContext folioExecutionContext;
  private final FolioModuleMetadata folioModuleMetadata;
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
  public List<User> getPrimaryUsersToLink() {
    return usersClient.getUserCollection(LINKABLE_USER_QUERY, 0, Integer.MAX_VALUE).getUsers();
  }

  @Override
  public List<User> getPrimaryUsersToLink(String tenantId) {
    return runInFolioContext(tenantId, folioModuleMetadata, folioExecutionContext, () -> getPrimaryUsersToLink());
  }

  @Override
  public void deleteById(String userId) {
    usersKeycloakClient.deleteUser(userId);
  }

  public User prepareShadowUser(UUID userId, String tenantId) {
    try (var ignored = new FolioExecutionContextSetter(
      prepareContextForTenant(tenantId, folioModuleMetadata, folioExecutionContext))) {
      log.info("prepareShadowUser:: Try to get user of tenant={} ", folioExecutionContext.getTenantId());

      var realUser = usersKeycloakClient.getUsersByUserId(userId.toString());
      if (Objects.isNull(realUser.getId())) {
        log.warn("Could not find real user with id: {} in his home tenant: {}", userId.toString(), tenantId);
        throw new ResourceNotFoundException(USER_ID, userId.toString());
      }
      UserType userType = Optional.ofNullable(realUser.getType()).map(UserType::fromName).orElse(null);
      if (NOT_APPLICABLE_USER_TYPES.contains(userType)) {
        log.warn("User with id: {} has type: {} which is not applicable for shadow user creation", userId.toString(), realUser.getType());
        throw new IllegalStateException("User type is not applicable for shadow user creation");
      }

      var shadowUser = new User();
      shadowUser.setId(userId.toString());
      shadowUser.setUsername(HelperUtils.generateShadowUsername(realUser.getUsername()));
      shadowUser.setType(UserType.SHADOW.getName());
      shadowUser.setActive(true);

      if (Objects.nonNull(realUser.getPersonal())) {
        // these firstname, lastname fields needed to correctly build UI metadata objects
        shadowUser.setPersonal(new Personal()
          .firstName(realUser.getPersonal().getFirstName())
          .lastName(realUser.getPersonal().getLastName())
          .email(realUser.getPersonal().getEmail())
          .preferredContactTypeId(realUser.getPersonal().getPreferredContactTypeId())
        );
      }

      shadowUser.setCustomFields(Map.of(ORIGINAL_TENANT_ID_REF_ID, tenantId));
      return shadowUser;
    }
  }
}
