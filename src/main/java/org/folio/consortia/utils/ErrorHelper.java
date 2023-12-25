package org.folio.consortia.utils;

import java.util.List;

import org.folio.consortia.domain.dto.Error;
import org.folio.consortia.domain.dto.Errors;

import feign.FeignException;
import lombok.experimental.UtilityClass;

@UtilityClass
public class ErrorHelper {

  public static Error createError(String message, ErrorType type, ErrorCode errorCode) {
    var error = new Error();
    error.setMessage(message);
    error.setType(type.getTypeCode());
    error.setCode(errorCode == null ? null : errorCode.name());
    return error;
  }


  public static Errors createErrors(Error error) {
    var e = new Errors();
    e.setErrors(List.of(error));
    return e;
  }

  public static Errors createUnknownError(String message) {
    return createErrors(createError(message, ErrorType.UNKNOWN, null));
  }

  public static Errors createInternalError(String message, ErrorCode errorCode) {
    return createErrors(createError(message, ErrorType.INTERNAL, errorCode));
  }

  public static Errors createExternalError(String message, ErrorCode errorCode) {
    return createErrors(createError(message, ErrorType.EXTERNAL, errorCode));
  }

  public static Errors createPermissionError(FeignException e, ErrorCode errorCode){
    String message = e.getMessage();
    String extractedMessage = extractPermissionFromErrorMessage(message);
    return createErrors(createError(extractedMessage, ErrorType.INTERNAL, errorCode));
  }

  private String extractPermissionFromErrorMessage(String e) {
    return e.substring(e.lastIndexOf('[') + 1, e.lastIndexOf(']')); // extract the main part from the error message
  }

  public enum ErrorType {
    EXTERNAL("-1"), // bad request or client error
    INTERNAL("-2"), // bad gateway or internal error (db error)
    FOLIO_EXTERNAL_OR_UNDEFINED("-3"),
    UNKNOWN("-4");

    private final String typeCode;

    ErrorType(String typeCode) {
      this.typeCode = typeCode;
    }

    public String getTypeCode() {
      return typeCode;
    }

  }

  public enum ErrorCode {
    VALIDATION_ERROR,
    NOT_FOUND_ERROR,
    DUPLICATE_ERROR,
    HAS_PRIMARY_AFFILIATION_ERROR,
    PUBLICATION_ERROR,
    PERMISSION_REQUIRED,
    BAD_GATEWAY,
    UNAUTHORIZED
  }

}
