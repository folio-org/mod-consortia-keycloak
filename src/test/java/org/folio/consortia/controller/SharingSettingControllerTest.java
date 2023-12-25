package org.folio.consortia.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.folio.consortia.service.SharingSettingService;
import java.util.UUID;

import org.folio.consortia.domain.dto.SharingSettingDeleteResponse;
import org.folio.consortia.domain.dto.SharingSettingResponse;
import org.folio.consortia.support.BaseIT;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;

class SharingSettingControllerTest extends BaseIT {
  @MockBean
  SharingSettingService sharingSettingService;

  @ParameterizedTest
  @ValueSource(strings = {"{\"settingId\":\"1844767a-8367-4926-9999-514c35840399\",\"url\":\"/organizations-storage/organizations\",\"payload\":{\"name\":\"ORG-NAME\",\"source\":\"local\"}}" })
  void shouldStartSharingSetting3(String body) throws Exception {
    var headers = defaultHeaders();
    UUID createSettingsPcId = UUID.randomUUID();
    UUID updateSettingsPcId = UUID.randomUUID();
    SharingSettingResponse sharingSettingResponse = new SharingSettingResponse()
      .createSettingsPCId(createSettingsPcId)
      .updateSettingsPCId(updateSettingsPcId);

    when(sharingSettingService.start(any(), any())).thenReturn(sharingSettingResponse);

    this.mockMvc.perform(
        post("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/sharing/settings")
          .headers(headers)
          .content(body)
          .contentType(MediaType.APPLICATION_JSON))
      .andExpect(status().isCreated())
      .andExpect(jsonPath("$.createSettingsPCId").value(String.valueOf(createSettingsPcId)))
      .andExpect(jsonPath("$.updateSettingsPCId").value(String.valueOf(updateSettingsPcId)));
  }

  @ParameterizedTest
  @ValueSource(strings = {"{\"settingId\":\"1844767a-8367-4926-9999-514c35840399\",\"url\":\"/organizations-storage/organizations\"}" })
  void shouldDeleteSharingSetting(String body) throws Exception {
    var headers = defaultHeaders();
    UUID pcId = UUID.randomUUID();
    SharingSettingDeleteResponse sharingSettingDeleteResponse = new SharingSettingDeleteResponse().pcId(pcId);

    when(sharingSettingService.delete(any(), any(), any())).thenReturn(sharingSettingDeleteResponse);

    this.mockMvc.perform(
        delete("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/sharing/settings/1844767a-8367-4926-9999-514c35840399")
          .headers(headers)
          .content(body)
          .contentType(MediaType.APPLICATION_JSON))
      .andExpect(status().is2xxSuccessful());
  }
}
