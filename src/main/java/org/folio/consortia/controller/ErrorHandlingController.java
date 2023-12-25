package org.folio.consortia.controller;

import java.util.List;
import java.util.Objects;

import org.folio.consortia.domain.dto.Error;
import org.folio.consortia.domain.dto.Errors;
import org.folio.consortia.exception.ConsortiumClientException;
import org.folio.consortia.exception.InvalidTokenException;
import org.folio.consortia.exception.UserAffiliationException;
import org.folio.consortia.exception.PublicationException;
import org.folio.consortia.exception.ResourceAlreadyExistException;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.utils.ErrorHelper;
import org.folio.consortia.utils.ErrorHelper.ErrorCode;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;

import feign.FeignException;
import lombok.extern.log4j.Log4j2;

@RestControllerAdvice
@Log4j2
public class ErrorHandlingController {

  @ResponseStatus(HttpStatus.NOT_FOUND)
  @ExceptionHandler(ResourceNotFoundException.class)
  public Errors handleNotFoundException(ResourceNotFoundException e) {
    return ErrorHelper.createExternalError(e.getMessage(), ErrorCode.NOT_FOUND_ERROR);
  }

  @ResponseStatus(HttpStatus.CONFLICT)
  @ExceptionHandler(ResourceAlreadyExistException.class)
  public Errors handleResourceAlreadyExistException(ResourceAlreadyExistException e) {
    return ErrorHelper.createExternalError(e.getMessage(), ErrorCode.DUPLICATE_ERROR);
  }

  @ResponseStatus(HttpStatus.CONFLICT)
  @ExceptionHandler({DataIntegrityViolationException.class})
  public Errors handleDataIntegrityViolationException(DataIntegrityViolationException e) {
    log.error("Handle data integrity violation", e);

    /*
    org.springframework.dao.DataIntegrityViolationException :-
    this is a generic data exception typically thrown by the Spring exception translation mechanism when dealing with lower level persistence exceptions.
    So to get clear error message we need to find rootCause first.
    */
    return ErrorHelper.createExternalError(Objects.requireNonNull(e.getRootCause()).getMessage(), ErrorCode.VALIDATION_ERROR);
  }

  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ExceptionHandler({
    MissingServletRequestParameterException.class,
    MethodArgumentTypeMismatchException.class,
    HttpMessageNotReadableException.class,
    IllegalArgumentException.class
  })
  public Errors handleValidationErrors(Exception e) {
    log.error("Handle validation errors", e);
    return ErrorHelper.createExternalError(e.getMessage(), ErrorCode.VALIDATION_ERROR);
  }

  @ResponseStatus(HttpStatus.UNAUTHORIZED)
  @ExceptionHandler({
    InvalidTokenException.class
  })
  public Errors handleTokenErrors(Exception e) {
    log.error("Handle token validation errors", e);
    return ErrorHelper.createExternalError(e.getMessage(), ErrorCode.UNAUTHORIZED);
  }

  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ExceptionHandler(UserAffiliationException.class)
  public Errors handleUserAffiliationException(Exception e) {
    return ErrorHelper.createExternalError(e.getMessage(), ErrorCode.VALIDATION_ERROR);
  }

  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ExceptionHandler(PublicationException.class)
  public Errors handlePublicationException(PublicationException e) {
    return ErrorHelper.createInternalError(e.getMessage(), ErrorCode.PUBLICATION_ERROR);
  }

  @ResponseStatus(HttpStatus.FORBIDDEN)
  @ExceptionHandler(ConsortiumClientException.class)
  public Errors handleConsortiumClientException(FeignException e) {
    log.error("Handle consortium client exception", e);
    return ErrorHelper.createPermissionError(e, ErrorCode.PERMISSION_REQUIRED);
  }

  @ResponseStatus(HttpStatus.BAD_GATEWAY)
  @ExceptionHandler(IllegalStateException.class)
  public Errors handleIllegalStateException(IllegalStateException e) {
    log.error("Handle illegal state exception", e);
    return ErrorHelper.createInternalError(e.getMessage(), ErrorCode.BAD_GATEWAY);
  }

  @ExceptionHandler(MethodArgumentNotValidException.class)
  @ResponseStatus(HttpStatus.UNPROCESSABLE_ENTITY)
  public Errors handleMethodArgumentNotValidException(MethodArgumentNotValidException ex) {
    log.error("Handle method argument not valid", ex);

    List<Error> errorList = ex.getBindingResult().getFieldErrors()
      .stream()
      .map(error -> {
        error.getObjectName();
        var customCode = error.getObjectName() + "ValidationError";
        return new Error()
          // Extract the error message and validation errors from the MethodArgumentNotValidException
          .message(String.format("'%s' validation failed. %s", error.getField(), error.getDefaultMessage()))
          .type(ErrorHelper.ErrorType.EXTERNAL.getTypeCode())
          .code(customCode);
      })
      .toList();

    return new Errors().errors(errorList);
  }

}
