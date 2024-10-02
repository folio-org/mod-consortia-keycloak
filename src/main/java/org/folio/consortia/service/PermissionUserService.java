package org.folio.consortia.service;

import org.folio.consortia.domain.dto.PermissionUser;

import java.util.Optional;

public interface PermissionUserService {

  /**
  * Gets permissionUser based on userId.
  *
  * @param userId  the id of user
  *
  * @return PermissionUser
  */
  Optional<PermissionUser> getByUserId(String userId);

  /**
  * Creates permissionUser with empty permissions list.
  * @param userId the id of user
  *
  * @return PermissionUser
  */
  PermissionUser createWithEmptyPermissions(String userId);

  /**
   * Creates permissionUser for userId with permission sets getting from file.
   *
   * @param userId                 the id of user
   * @param permissionSetsFilePath the path of file includes permission set names to add
   * @return PermissionUser
   */
  PermissionUser createWithPermissionSetsFromFile(String userId, String permissionSetsFilePath);

  /**
   * Remove user permissions from permission_users table based on userId
   *
   * @param userId id of user
   */
  void deletePermissionUser(String userId);
}
