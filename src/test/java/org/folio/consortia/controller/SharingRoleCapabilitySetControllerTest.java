package org.folio.consortia.controller;

import static org.folio.consortia.support.EntityUtils.SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE;
import static org.folio.consortia.utils.InputOutputTestUtils.getMockDataAsString;
import static org.hamcrest.Matchers.hasItems;
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
  private static final String BASE_URL = "/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/sharing/roles";

  @MockBean
  SharingRoleCapabilitySetService sharingRoleCapabilitySetService;

  @Test
  void shouldStartSharingRole() throws Exception {
    var headers = defaultHeaders();
    String request = getMockDataAsString(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE);
    var createPcIds = List.of(UUID.randomUUID(), UUID.randomUUID());
    var updatePcIds = List.of(UUID.randomUUID(), UUID.randomUUID());
    var response = new SharingRoleCapabilitySetResponse()
      .createPCIds(createPcIds)
      .updatePCIds(updatePcIds);

    when(sharingRoleCapabilitySetService.start(any(), any())).thenReturn(response);

    this.mockMvc.perform(
        post(BASE_URL + "/capability-sets")
          .headers(headers)
          .content(request)
          .contentType(MediaType.APPLICATION_JSON))
      .andExpect(status().isCreated())
      .andExpect(jsonPath("$.createPCIds").isArray())
      .andExpect(jsonPath("$.createPCIds", hasItems(createPcIds.get(0).toString(), createPcIds.get(1).toString())))
      .andExpect(jsonPath("$.updatePCIds").isArray())
      .andExpect(jsonPath("$.updatePCIds", hasItems(updatePcIds.get(0).toString(), updatePcIds.get(1).toString())));
  }

  @Test
  void shouldDeleteSharingRole() throws Exception {
    var headers = defaultHeaders();
    String request = getMockDataAsString(SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE);
    var pcIds = List.of(UUID.randomUUID(), UUID.randomUUID());
    var response = new SharingRoleCapabilitySetDeleteResponse()
      .pcIds(pcIds);

    when(sharingRoleCapabilitySetService.delete(any(), any(), any())).thenReturn(response);

    this.mockMvc.perform(
        delete(BASE_URL + "/2844767a-8367-4926-9999-514c35840399/capability-sets")
          .headers(headers)
          .content(request)
          .contentType(MediaType.APPLICATION_JSON))
      .andExpect(status().is2xxSuccessful())
      .andExpect(jsonPath("$.pcIds").isArray())
      .andExpect(jsonPath("$.pcIds", hasItems(pcIds.get(0).toString(), pcIds.get(1).toString())));
  }
}
