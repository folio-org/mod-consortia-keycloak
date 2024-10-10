package org.folio.consortia.controller;

import static org.folio.consortia.support.EntityUtils.SHARING_ROLE_REQUEST_SAMPLE;
import static org.folio.consortia.support.EntityUtils.SHARING_ROLE_REQUEST_SAMPLE_WITHOUT_PAYLOAD;
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
import org.folio.consortia.domain.dto.SharingRoleDeleteResponse;
import org.folio.consortia.domain.dto.SharingRoleResponse;
import org.folio.consortia.service.impl.SharingRoleService;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;

class SharingRoleControllerTest extends BaseIT {
  private static final String BASE_URL = "/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/sharing/roles";

  @MockBean
  SharingRoleService sharingRoleService;

  @Test
  void shouldStartSharingRole() throws Exception {
    var body = getMockDataAsString(SHARING_ROLE_REQUEST_SAMPLE);
    var headers = defaultHeaders();
    var createPcIds = List.of(UUID.randomUUID(), UUID.randomUUID());
    var updatePcIds = List.of(UUID.randomUUID(), UUID.randomUUID());
    var sharingRoleResponse = new SharingRoleResponse()
      .createPCIds(createPcIds)
      .updatePCIds(updatePcIds);

    when(sharingRoleService.start(any(), any())).thenReturn(sharingRoleResponse);

    this.mockMvc.perform(
        post(BASE_URL)
          .headers(headers)
          .content(body)
          .contentType(MediaType.APPLICATION_JSON))
      .andExpect(status().isCreated())
      .andExpect(jsonPath("$.createPCIds").isArray())
      .andExpect(jsonPath("$.createPCIds", hasItems(createPcIds.get(0).toString(), createPcIds.get(1).toString())))
      .andExpect(jsonPath("$.updatePCIds").isArray())
      .andExpect(jsonPath("$.updatePCIds", hasItems(updatePcIds.get(0).toString(), updatePcIds.get(1).toString())));

  }

  @Test
  void shouldDeleteSharingRole() throws Exception {
    var body = getMockDataAsString(SHARING_ROLE_REQUEST_SAMPLE_WITHOUT_PAYLOAD);
    var headers = defaultHeaders();
    var pcIds = List.of(UUID.randomUUID(), UUID.randomUUID());
    var sharingRoleDeleteResponse = new SharingRoleDeleteResponse().pcIds(pcIds);

    when(sharingRoleService.delete(any(), any(), any())).thenReturn(sharingRoleDeleteResponse);

    this.mockMvc.perform(
        delete(BASE_URL + "/3844767a-8367-4926-9999-514c35840399")
          .headers(headers)
          .content(body)
          .contentType(MediaType.APPLICATION_JSON))
      .andExpect(status().is2xxSuccessful())
      .andExpect(jsonPath("$.pcIds").isArray())
      .andExpect(jsonPath("$.pcIds", hasItems(pcIds.get(0).toString(), pcIds.get(1).toString())));
  }
}
