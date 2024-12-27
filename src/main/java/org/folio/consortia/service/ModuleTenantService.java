package org.folio.consortia.service;

public interface ModuleTenantService {
  /**
   * Retrieves the module ID for the "mod-users" module.
   * This method determines the appropriate module ID based on the platform configuration
   * and the tenant context.
   *
   * @return the module ID for "mod-users"
   * @throws org.folio.spring.exception.NotFoundException if the module ID for "mod-users" is not found
   */
  String getModUsersModuleId();
}
