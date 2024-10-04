package org.folio.consortia.controller;

import org.folio.consortia.base.BaseIT;
import org.folio.consortia.domain.dto.SharingRoleCapabilityDeleteResponse;
import org.folio.consortia.domain.dto.SharingRoleCapabilityResponse;
import org.folio.consortia.service.impl.SharingRoleCapabilityService;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.UUID;

import static org.folio.consortia.support.EntityUtils.SHARING_ROLE_CAPABILITIES_REQUEST_SAMPLE;
import static org.folio.consortia.utils.InputOutputTestUtils.getMockDataAsString;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

class SharingRoleCapabilityControllerTest extends BaseIT {

  @MockBean
  SharingRoleCapabilityService sharingRoleCapabilityService;

  @Test
  void shouldStartSharingRoleCapabilities() throws Exception {
    var headers = defaultHeaders();
    var request = getMockDataAsString(SHARING_ROLE_CAPABILITIES_REQUEST_SAMPLE);
    var createPcId = UUID.randomUUID();
    var updatePcId = UUID.randomUUID();
    var response = new SharingRoleCapabilityResponse()
      .createPCIds(List.of(createPcId))
      .updatePCIds(List.of(updatePcId));

    when(sharingRoleCapabilityService.start(any(), any())).thenReturn(response);

    this.mockMvc.perform(
        post("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/sharing/roles/capabilities")
          .headers(headers)
          .content(request)
          .contentType(MediaType.APPLICATION_JSON))
      .andExpect(status().isCreated())
      .andExpect(jsonPath("$.createRoleCapabilitiesPCId").value(String.valueOf(createPcId)))
      .andExpect(jsonPath("$.updateRoleCapabilitiesPCId").value(String.valueOf(updatePcId)));
  }

  @Test
  void shouldDeleteSharingRoleCapabilities() throws Exception {
    var headers = defaultHeaders();
    var request = getMockDataAsString(SHARING_ROLE_CAPABILITIES_REQUEST_SAMPLE);
    var pcId = UUID.randomUUID();
    var response = new SharingRoleCapabilityDeleteResponse()
      .pcIds(List.of(pcId));

    when(sharingRoleCapabilityService.delete(any(), any(), any())).thenReturn(response);

    this.mockMvc.perform(
        delete("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/" +
          "sharing/roles/2844767a-8367-4926-9999-514c35840399/capabilities")
          .headers(headers)
          .content(request)
          .contentType(MediaType.APPLICATION_JSON))
      .andExpect(status().is2xxSuccessful());
  }
}
