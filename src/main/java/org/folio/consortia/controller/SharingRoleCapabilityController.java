package org.folio.consortia.controller;

import lombok.RequiredArgsConstructor;
import org.folio.consortia.domain.dto.SharingRoleCapabilityDeleteResponse;
import org.folio.consortia.domain.dto.SharingRoleCapabilityRequest;
import org.folio.consortia.domain.dto.SharingRoleCapabilityResponse;
import org.folio.consortia.rest.resource.RoleCapabilitiesApi;
import org.folio.consortia.service.impl.SharingRoleCapabilityService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.UUID;

import static org.springframework.http.HttpStatus.CREATED;
import static org.springframework.http.HttpStatus.OK;

@RestController
@RequestMapping("/consortia/{consortiumId}/sharing")
@RequiredArgsConstructor
public class SharingRoleCapabilityController implements RoleCapabilitiesApi {

  private final SharingRoleCapabilityService sharingRoleCapabilityService;

  @Override
  public ResponseEntity<SharingRoleCapabilityResponse> startSharingRoleCapabilities(UUID consortiumId,
                                                                                    SharingRoleCapabilityRequest request) {
    return ResponseEntity
      .status(CREATED)
      .body(sharingRoleCapabilityService.start(consortiumId, request));
  }

  @Override
  public ResponseEntity<SharingRoleCapabilityDeleteResponse> deleteSharingRoleCapabilities(UUID consortiumId,
                                                                                           UUID roleId,
                                                                                           SharingRoleCapabilityRequest request) {
    return ResponseEntity
      .status(OK)
      .body(sharingRoleCapabilityService.delete(consortiumId, roleId, request));
  }
}
