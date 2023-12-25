package org.folio.consortia.controller;

import static org.springframework.http.HttpStatus.CREATED;

import org.folio.consortia.service.SharingSettingService;
import java.util.UUID;
import lombok.RequiredArgsConstructor;
import org.folio.consortia.domain.dto.SharingSettingDeleteResponse;
import org.folio.consortia.domain.dto.SharingSettingRequest;
import org.folio.consortia.domain.dto.SharingSettingResponse;
import org.folio.consortia.rest.resource.SettingsApi;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/consortia/{consortiumId}/sharing")
@RequiredArgsConstructor
public class SharingSettingController implements SettingsApi {

  private final SharingSettingService sharingSettingService;

  @Override
  public ResponseEntity<SharingSettingResponse> startSharingSetting(UUID consortiumId, SharingSettingRequest sharingSettingRequest) {
    return ResponseEntity.status(CREATED).body(sharingSettingService.start(consortiumId, sharingSettingRequest));
  }

  @Override
  public ResponseEntity<SharingSettingDeleteResponse> deleteSharingSetting(UUID consortiumId, UUID settingId,
                                                                           SharingSettingRequest sharingSettingRequest) {
    return ResponseEntity.ok(sharingSettingService.delete(consortiumId, settingId, sharingSettingRequest));
  }
}
