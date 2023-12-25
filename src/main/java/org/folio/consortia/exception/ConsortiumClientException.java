package org.folio.consortia.exception;

public class ConsortiumClientException extends RuntimeException {

  public ConsortiumClientException(Exception exception) {
    super(exception);
  }
}
