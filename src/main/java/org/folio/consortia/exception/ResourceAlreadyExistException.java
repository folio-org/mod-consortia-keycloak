package org.folio.consortia.exception;

public class ResourceAlreadyExistException extends RuntimeException{
  private static final String RESOURCE_EXIST_MSG_TEMPLATE = "Object with %s [%s] is already presented in the system";

  public ResourceAlreadyExistException(String attribute, String value) {
    super(String.format(RESOURCE_EXIST_MSG_TEMPLATE, attribute, value));
  }

  public ResourceAlreadyExistException(String errorMsg) {
    super(String.format(errorMsg));
  }
}
