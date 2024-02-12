package org.folio.consortia.controller;

import java.util.UUID;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.consortia.domain.dto.Payload;
import org.folio.consortia.domain.dto.UserTenantCollection;
import org.folio.consortia.rest.resource.SelfApi;
import org.folio.consortia.service.UserTenantService;
import org.folio.consortia.utils.TokenUtils;
import org.folio.spring.FolioExecutionContext;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

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
    Payload payload = TokenUtils.validateAndParseTokenPayload(token);

    UUID userId = folioExecutionContext.getUserId() != null ? folioExecutionContext.getUserId() : payload.getUserId();

    UserTenantCollection userTenantCollection = userTenantService.getByUserId(consortiumId, userId, 0, Integer.MAX_VALUE);

    return ResponseEntity.ok(userTenantCollection);
  }
}
