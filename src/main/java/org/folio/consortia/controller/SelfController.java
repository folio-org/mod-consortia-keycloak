package org.folio.consortia.controller;

import lombok.extern.log4j.Log4j2;
import lombok.RequiredArgsConstructor;
import org.folio.consortia.domain.dto.UserTenantCollection;
import org.folio.consortia.exception.InvalidTokenException;
import org.folio.consortia.rest.resource.SelfApi;
import org.folio.consortia.service.UserTenantService;
import org.folio.consortia.utils.TokenUtils;
import org.folio.spring.FolioExecutionContext;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.UUID;

@RestController
@RequestMapping("/consortia/{consortiumId}")
@Log4j2
@RequiredArgsConstructor
public class SelfController implements SelfApi {

  private final UserTenantService userTenantService;
  private final FolioExecutionContext folioExecutionContext;

  @Override
  public ResponseEntity<UserTenantCollection> getUserTenantsForCurrentUser(UUID consortiumId) {
    String token = folioExecutionContext.getToken();
    UUID userId = folioExecutionContext.getUserId();

    if (!TokenUtils.isValid(token)) {
      throw new InvalidTokenException();
    }

    UserTenantCollection userTenantCollection = userTenantService.getByUserId(consortiumId, userId, 0, Integer.MAX_VALUE);

    return ResponseEntity.ok(userTenantCollection);
  }
}
