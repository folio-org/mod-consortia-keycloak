package org.folio.consortia.exception;

public class PublicationException extends RuntimeException {

  public static final String TENANT_LIST_EMPTY = "Tenant list is empty";
  public static final String PRIMARY_AFFILIATION_NOT_EXISTS = "User doesn't have primary affiliation";
  public PublicationException(String message) {
    super(message);
  }

  public PublicationException(Throwable cause) {
    super(cause);
  }
}
