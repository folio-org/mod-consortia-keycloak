package org.folio.consortia.exception;

public class UserAffiliationException extends RuntimeException {
  public static final String USER_HAS_PRIMARY_AFFILIATION_WITH_TENANT = "User with id [%s] has primary affiliation" +
    " with tenant [%s]. Primary Affiliation cannot be deleted";
  public static final String AFFILIATION_FROM_CENTRAL_TENANT_CAN_NOT_BE_DELETED = "User with id [%s] has required affiliation in central" +
      " tenant [%s]. It cannot be deleted";

  public UserAffiliationException(String message) {
    super(message);
  }
}
