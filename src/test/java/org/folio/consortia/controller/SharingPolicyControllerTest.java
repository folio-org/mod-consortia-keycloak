package org.folio.consortia.controller;

import static org.folio.consortia.support.EntityUtils.SHARING_POLICY_REQUEST_SAMPLE_FOR_ROLES;
import static org.folio.consortia.utils.InputOutputTestUtils.getMockDataAsString;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.UUID;

import org.folio.consortia.base.BaseIT;
import org.folio.consortia.domain.dto.SharingPolicyDeleteResponse;
import org.folio.consortia.domain.dto.SharingPolicyResponse;
import org.folio.consortia.service.impl.SharingPolicyService;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;

class SharingPolicyControllerTest extends BaseIT {
  private static final String BASE_URL = "/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/sharing/policies";

  @MockBean
  SharingPolicyService sharingPolicyService;

  @Test
  void shouldStartSharingPolicy() throws Exception {
    var headers = defaultHeaders();
    var body = getMockDataAsString(SHARING_POLICY_REQUEST_SAMPLE_FOR_ROLES);
    UUID createPoliciesPcId = UUID.randomUUID();
    UUID updatePoliciesPcId = UUID.randomUUID();
    SharingPolicyResponse sharingPolicyResponse = new SharingPolicyResponse()
      .createPCId(createPoliciesPcId)
      .updatePCId(updatePoliciesPcId);

    when(sharingPolicyService.start(any(), any())).thenReturn(sharingPolicyResponse);

    this.mockMvc.perform(
        post(BASE_URL)
          .headers(headers)
          .content(body)
          .contentType(MediaType.APPLICATION_JSON))
      .andExpect(status().isCreated())
      .andExpect(jsonPath("$.createPCId").value(String.valueOf(createPoliciesPcId)))
      .andExpect(jsonPath("$.updatePCId").value(String.valueOf(updatePoliciesPcId)));
  }

  @Test
  void shouldDeleteSharingPolicy() throws Exception {
    var headers = defaultHeaders();
    var body = getMockDataAsString(SHARING_POLICY_REQUEST_SAMPLE_FOR_ROLES);
    UUID pcId = UUID.randomUUID();
    SharingPolicyDeleteResponse sharingPolicyDeleteResponse = new SharingPolicyDeleteResponse().pcId(pcId);

    when(sharingPolicyService.delete(any(), any(), any())).thenReturn(sharingPolicyDeleteResponse);

    this.mockMvc.perform(
        delete(BASE_URL + "/1844767a-8367-4926-9999-514c35840399")
          .headers(headers)
          .content(body)
          .contentType(MediaType.APPLICATION_JSON))
      .andExpect(status().is2xxSuccessful());
  }
}
