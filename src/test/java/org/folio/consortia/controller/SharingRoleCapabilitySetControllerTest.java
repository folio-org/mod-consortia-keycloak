package org.folio.consortia.controller;

import static org.folio.consortia.support.EntityUtils.SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE;
import static org.folio.consortia.utils.InputOutputTestUtils.getMockDataAsString;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.List;
import java.util.UUID;
import org.folio.consortia.base.BaseIT;
import org.folio.consortia.domain.dto.SharingRoleCapabilitySetDeleteResponse;
import org.folio.consortia.domain.dto.SharingRoleCapabilitySetResponse;
import org.folio.consortia.service.impl.SharingRoleCapabilitySetService;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;

class SharingRoleCapabilitySetControllerTest extends BaseIT {

  @MockBean
  SharingRoleCapabilitySetService sharingRoleCapabilitySetService;

  @Test
  void shouldStartSharingRole() throws Exception {
    var headers = defaultHeaders();
    String request = getMockDataAsString(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE);
    UUID createRoleCapabilitySetsPCId = UUID.randomUUID();
    UUID updateRoleCapabilitySetsPCId = UUID.randomUUID();
    var response = new SharingRoleCapabilitySetResponse()
      .createPCIds(List.of(createRoleCapabilitySetsPCId))
      .updatePCIds(List.of(updateRoleCapabilitySetsPCId));

    when(sharingRoleCapabilitySetService.start(any(), any())).thenReturn(response);

    this.mockMvc.perform(
        post("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/sharing/roles/capability-sets")
          .headers(headers)
          .content(request)
          .contentType(MediaType.APPLICATION_JSON))
      .andExpect(status().isCreated())
      .andExpect(jsonPath("$.createRoleCapabilitySetsPCId").value(String.valueOf(createRoleCapabilitySetsPCId)))
      .andExpect(jsonPath("$.updateRoleCapabilitySetsPCId").value(String.valueOf(updateRoleCapabilitySetsPCId)));
  }

  @Test
  void shouldDeleteSharingRole() throws Exception {
    var headers = defaultHeaders();
    String request = getMockDataAsString(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE);
    UUID pcId = UUID.randomUUID();
    var response = new SharingRoleCapabilitySetDeleteResponse()
      .pcIds(List.of(pcId));

    when(sharingRoleCapabilitySetService.delete(any(), any(), any())).thenReturn(response);

    this.mockMvc.perform(
        delete("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/" +
          "sharing/roles/2844767a-8367-4926-9999-514c35840399/capability-sets")
          .headers(headers)
          .content(request)
          .contentType(MediaType.APPLICATION_JSON))
      .andExpect(status().is2xxSuccessful());
  }
}
