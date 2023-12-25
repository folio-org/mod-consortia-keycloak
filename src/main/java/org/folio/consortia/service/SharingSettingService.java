package org.folio.consortia.service;

import java.util.UUID;

import org.folio.consortia.domain.dto.SharingSettingDeleteResponse;
import org.folio.consortia.domain.dto.SharingSettingRequest;
import org.folio.consortia.domain.dto.SharingSettingResponse;

public interface SharingSettingService {

  /**
   * Start sharing setting
   * @param consortiumId UUID of consortium entity
   * @param sharingSettingRequest the sharingSettingDTO (data transfer object)
   * @return SharingInstanceDto
   */
  SharingSettingResponse start(UUID consortiumId, SharingSettingRequest sharingSettingRequest);

  /**
   * Delete sharing setting for all tenants
   * @param consortiumId ID of consortium
   * @param settingId ID of setting
   * @param sharingSettingRequest the sharingSettingDTO (data transfer object)
   * @return Sharing setting response for delete operation
   */
  SharingSettingDeleteResponse delete(UUID consortiumId, UUID settingId, SharingSettingRequest sharingSettingRequest);
}
