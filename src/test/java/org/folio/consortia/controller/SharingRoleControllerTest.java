package org.folio.consortia.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.UUID;

import org.folio.consortia.base.BaseIT;
import org.folio.consortia.domain.dto.SharingRoleDeleteResponse;
import org.folio.consortia.domain.dto.SharingRoleResponse;
import org.folio.consortia.service.impl.SharingRoleService;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;

class SharingRoleControllerTest extends BaseIT {
  @MockBean
  SharingRoleService sharingRoleService;

  @ParameterizedTest
  @ValueSource(strings = {"{\"roleId\":\"2844767a-8367-4926-9999-514c35840399\",\"url\":\"/role\",\"payload\":{\"name\":\"ROLE-NAME\",\"source\":\"local\"}}" })
  void shouldStartSharingRole(String body) throws Exception {
    var headers = defaultHeaders();
    UUID createRolesPcId = UUID.randomUUID();
    UUID updateRolesPcId = UUID.randomUUID();
    SharingRoleResponse sharingRoleResponse = new SharingRoleResponse()
      .createRolesPCId(createRolesPcId)
      .updateRolesPCId(updateRolesPcId);

    when(sharingRoleService.start(any(), any())).thenReturn(sharingRoleResponse);

    this.mockMvc.perform(
        post("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/sharing/roles")
          .headers(headers)
          .content(body)
          .contentType(MediaType.APPLICATION_JSON))
      .andExpect(status().isCreated())
      .andExpect(jsonPath("$.createRolesPCId").value(String.valueOf(createRolesPcId)))
      .andExpect(jsonPath("$.updateRolesPCId").value(String.valueOf(updateRolesPcId)));
  }

  @ParameterizedTest
  @ValueSource(strings = {"{\"roleId\":\"2844767a-8367-4926-9999-514c35840399\",\"url\":\"/role\"}" })
  void shouldDeleteSharingRole(String body) throws Exception {
    var headers = defaultHeaders();
    UUID pcId = UUID.randomUUID();
    SharingRoleDeleteResponse sharingRoleDeleteResponse = new SharingRoleDeleteResponse().pcId(pcId);

    when(sharingRoleService.delete(any(), any(), any())).thenReturn(sharingRoleDeleteResponse);

    this.mockMvc.perform(
        delete("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/sharing/roles/2844767a-8367-4926-9999-514c35840399")
          .headers(headers)
          .content(body)
          .contentType(MediaType.APPLICATION_JSON))
      .andExpect(status().is2xxSuccessful());
  }
}
