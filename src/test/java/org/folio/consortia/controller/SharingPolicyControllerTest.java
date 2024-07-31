package org.folio.consortia.controller;

import org.folio.consortia.base.BaseIT;
import org.folio.consortia.domain.dto.SharingPolicyDeleteResponse;
import org.folio.consortia.domain.dto.SharingPolicyResponse;
import org.folio.consortia.service.impl.SharingPolicyService;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;

import java.util.UUID;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

class SharingPolicyControllerTest extends BaseIT {
  @MockBean
  SharingPolicyService sharingPolicyService;

  @ParameterizedTest
  @ValueSource(strings = {"{\"policyId\":\"1844767a-8367-4926-9999-514c35840399\",\"url\":\"/organizations-storage/organizations\",\"payload\":{\"name\":\"ORG-NAME\",\"source\":\"local\"}}" })
  void shouldStartSharingPolicy(String body) throws Exception {
    var headers = defaultHeaders();
    UUID createPoliciesPcId = UUID.randomUUID();
    UUID updatePoliciesPcId = UUID.randomUUID();
    SharingPolicyResponse sharingPolicyResponse = new SharingPolicyResponse()
      .createPoliciesPCId(createPoliciesPcId)
      .updatePoliciesPCId(updatePoliciesPcId);

    when(sharingPolicyService.start(any(), any())).thenReturn(sharingPolicyResponse);

    this.mockMvc.perform(
        post("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/sharing/policies")
          .headers(headers)
          .content(body)
          .contentType(MediaType.APPLICATION_JSON))
      .andExpect(status().isCreated())
      .andExpect(jsonPath("$.createPoliciesPCId").value(String.valueOf(updatePoliciesPcId)))
      .andExpect(jsonPath("$.updatePoliciesPCId").value(String.valueOf(updatePoliciesPcId)));
  }

  @ParameterizedTest
  @ValueSource(strings = {"{\"policyId\":\"1844767a-8367-4926-9999-514c35840399\",\"url\":\"/organizations-storage/organizations\"}" })
  void shouldDeleteSharingPolicy(String body) throws Exception {
    var headers = defaultHeaders();
    UUID pcId = UUID.randomUUID();
    SharingPolicyDeleteResponse sharingPolicyDeleteResponse = new SharingPolicyDeleteResponse().pcId(pcId);

    when(sharingPolicyService.delete(any(), any(), any())).thenReturn(sharingPolicyDeleteResponse);

    this.mockMvc.perform(
        delete("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/sharing/polices/1844767a-8367-4926-9999-514c35840399")
          .headers(headers)
          .content(body)
          .contentType(MediaType.APPLICATION_JSON))
      .andExpect(status().is2xxSuccessful());
  }
}
