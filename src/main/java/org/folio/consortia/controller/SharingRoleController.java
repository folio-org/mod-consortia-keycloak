package org.folio.consortia.controller;

import static org.springframework.http.HttpStatus.CREATED;
import static org.springframework.http.HttpStatus.OK;

import java.util.UUID;

import lombok.RequiredArgsConstructor;
import org.folio.consortia.domain.dto.SharingRoleDeleteResponse;
import org.folio.consortia.domain.dto.SharingRoleRequest;
import org.folio.consortia.domain.dto.SharingRoleResponse;
import org.folio.consortia.rest.resource.RolesApi;
import org.folio.consortia.service.impl.SharingRoleService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/consortia/{consortiumId}/sharing")
@RequiredArgsConstructor
public class SharingRoleController implements RolesApi {

  private final SharingRoleService sharingRoleService;

  @Override
  public ResponseEntity<SharingRoleResponse> startSharingRole(UUID consortiumId, SharingRoleRequest sharingRoleRequest) {
    return ResponseEntity
      .status(CREATED)
      .body(sharingRoleService.start(consortiumId, sharingRoleRequest));
  }

  @Override
  public ResponseEntity<SharingRoleDeleteResponse> deleteSharingRole(UUID consortiumId, UUID roleId,
                                                                     SharingRoleRequest sharingRoleRequest) {
    return ResponseEntity
      .status(OK)
      .body(sharingRoleService.delete(consortiumId, roleId, sharingRoleRequest));
  }
}
