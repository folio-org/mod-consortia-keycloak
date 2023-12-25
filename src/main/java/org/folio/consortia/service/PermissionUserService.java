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
   * Creates permissionUser for userId with permissions getting from file.
   *
   * @param userId  the id of user
   * @param permissionsFilePath  the path of file includes permission names to add
   *
   * @return PermissionUser
   */
  PermissionUser createWithPermissionsFromFile(String userId, String permissionsFilePath);

  /**
   * Add permissions for existed permission user.
   *
   * @param permissionUser  the permissionUser
   * @param permissionsFilePath  the path of file includes permission names to add
   */
  void addPermissions(PermissionUser permissionUser, String permissionsFilePath);

  /**
   * Remove user permissions from permission_users table based on userId
   *
   * @param userId id of user
   */
  void deletePermissionUser(String userId);
}
