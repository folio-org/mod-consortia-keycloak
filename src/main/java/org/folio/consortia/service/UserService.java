package org.folio.consortia.service;

import org.folio.consortia.domain.dto.User;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface UserService {

  /**
   * Creates user.
   *
   * @param user user.
   *
   * @return user
   */

  User createUser(User user);

  /**
   * Updates user.
   *
   * @param user user.
   */
   void updateUser(User user);

  /**
   * Get existing user by id.
   *
   * @param userId id of user.
   *
   * @return user
   */
  User getById(UUID userId);

  /**
   * Get existing user by username.
   * @param username username of user.
   * @return user
   */
  Optional<User> getByUsername(String username);

  /**
   * Get primary users of valid type for linking/affiliation sync
   *
   * @return list of users.
   */
  List<User> getPrimaryUsersToLink();

  /**
   * Get primary users of valid type for linking/affiliation sync associated with tenant
   *
   * @param tenantId id of tenant.
   * @return list of users.
   */
  List<User> getPrimaryUsersToLink(String tenantId);

  /**
   * Deletes existing user by id.
   *
   * @param userId id of user.
   */
  void deleteById(String userId);

  /**
   * Prepare shadow user from real user.
   *
   * @param userId id of "real" user.
   * @param tenantId id of tenant.
   *
   * return user.
   */
  User prepareShadowUser(UUID userId, String tenantId);
}
