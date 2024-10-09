package org.folio.consortia.service;

public interface CapabilitiesUserService {

  /**
   * Creates permissionUser for userId with permission sets getting from file.
   *
   * @param userId                 the id of user
   * @param permissionSetsFilePath the path of file includes permission set names to add
   */
  void createWithPermissionSetsFromFile(String userId, String permissionSetsFilePath);

  /**
   * Remove user permissions from permission_users table based on userId
   *
   * @param userId id of user
   */
  void deletePermissionUser(String userId);
}
