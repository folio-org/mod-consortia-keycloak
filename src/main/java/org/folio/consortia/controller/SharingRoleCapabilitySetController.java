package org.folio.consortia.controller;

import static org.springframework.http.HttpStatus.CREATED;
import static org.springframework.http.HttpStatus.OK;

import java.util.UUID;
import lombok.RequiredArgsConstructor;
import org.folio.consortia.domain.dto.SharingRoleCapabilitySetDeleteResponse;
import org.folio.consortia.domain.dto.SharingRoleCapabilitySetRequest;
import org.folio.consortia.rest.resource.RoleCapabilitySetApi;
import org.folio.consortia.domain.dto.SharingRoleCapabilitySetResponse;
import org.folio.consortia.service.impl.SharingRoleCapabilitySetService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/consortia/{consortiumId}/sharing")
@RequiredArgsConstructor
public class SharingRoleCapabilitySetController implements RoleCapabilitySetApi {

  private final SharingRoleCapabilitySetService sharingRoleCapabilitySetService;

  @Override
  public ResponseEntity<SharingRoleCapabilitySetResponse> startSharingRoleCapabilitySet(UUID consortiumId,
                                                                                        SharingRoleCapabilitySetRequest sharingRoleCapabilitySetRequest) {
    return ResponseEntity
      .status(CREATED)
      .body(sharingRoleCapabilitySetService.start(consortiumId, sharingRoleCapabilitySetRequest));
  }

  @Override
  public ResponseEntity<SharingRoleCapabilitySetDeleteResponse> deleteSharingRoleCapabilitySet(UUID consortiumId, UUID roleId,
                                                                                               SharingRoleCapabilitySetRequest sharingRoleCapabilitySetRequest) {
    return ResponseEntity
      .status(OK)
      .body(sharingRoleCapabilitySetService.delete(consortiumId, roleId, sharingRoleCapabilitySetRequest));
  }
}
