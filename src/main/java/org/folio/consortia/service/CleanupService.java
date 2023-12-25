package org.folio.consortia.service;

public interface CleanupService {

  /**
   * <p>
   * Removes all old records from the tables 'pc_state' and 'pc_tenant_request'
   * </p>
   * <p>
   * Use <em>_timer</em> interface configuration in <em>ModuleDescriptor.json</em> to define interval of the job execution.
   * </p>
   * <p>
   * Change system property <em>publication-records-max-age-in-seconds</em> in order to define new maximal age of the records
   * for removal
   * </p>
   */
  void clearPublicationTables();

}
