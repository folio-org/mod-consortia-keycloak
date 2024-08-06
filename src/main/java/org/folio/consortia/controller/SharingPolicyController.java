package org.folio.consortia.controller;

import lombok.RequiredArgsConstructor;
import org.folio.consortia.domain.dto.SharingPolicyDeleteResponse;
import org.folio.consortia.domain.dto.SharingPolicyRequest;
import org.folio.consortia.domain.dto.SharingPolicyResponse;
import org.folio.consortia.rest.resource.PoliciesApi;
import org.folio.consortia.service.impl.SharingPolicyService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.UUID;

import static org.springframework.http.HttpStatus.CREATED;
import static org.springframework.http.HttpStatus.OK;

@RestController
@RequestMapping("/consortia/{consortiumId}/sharing")
@RequiredArgsConstructor
public class SharingPolicyController implements PoliciesApi {

  private final SharingPolicyService sharingPolicyService;

  @Override
  public ResponseEntity<SharingPolicyResponse> startSharingPolicy(UUID consortiumId,
                                                                  SharingPolicyRequest sharingPolicyRequest) {
    return ResponseEntity
      .status(CREATED)
      .body(sharingPolicyService.start(consortiumId, sharingPolicyRequest));
  }

  @Override
  public ResponseEntity<SharingPolicyDeleteResponse> deleteSharingPolicy(UUID consortiumId, UUID policyId,
                                                                         SharingPolicyRequest sharingPolicyRequest) {
    return ResponseEntity
      .status(OK)
      .body(sharingPolicyService.delete(consortiumId, policyId, sharingPolicyRequest));
  }

}
