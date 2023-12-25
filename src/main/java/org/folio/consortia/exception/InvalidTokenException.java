package org.folio.consortia.exception;

public class InvalidTokenException extends RuntimeException {
  private static final String INVALID_TOKEN = "Invalid token";

  public InvalidTokenException() {
    super(INVALID_TOKEN);
  }

}
