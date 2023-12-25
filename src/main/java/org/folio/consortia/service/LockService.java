package org.folio.consortia.service;

public interface LockService {

  /**
   * Lock tenant setup inside transaction. Unlock is done automatically at the transaction end.
   */
  void lockTenantSetupWithinTransaction();
}
