package org.folio.consortia.controller;

import static org.folio.consortia.support.TestConstants.ACTION_ID;
import static org.folio.consortia.support.TestConstants.CONSORTIUM_ID;
import static org.folio.consortia.support.TestConstants.INSTANCE_ID;
import static org.folio.consortia.support.EntityUtils.createSharingInstance;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.folio.consortia.repository.ConsortiumRepository;
import org.folio.consortia.repository.SharingInstanceRepository;
import org.folio.consortia.repository.TenantRepository;
import org.folio.consortia.service.ConsortiaConfigurationService;
import org.folio.consortia.service.SharingInstanceService;
import java.util.UUID;

import org.folio.consortia.domain.dto.SharingInstance;
import org.folio.consortia.base.BaseIT;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.http.MediaType;
import org.springframework.test.context.bean.override.mockito.MockitoBean;

class SharingInstanceControllerTest extends BaseIT {

  @MockitoBean
  private SharingInstanceService sharingInstanceService;
  @MockitoBean
  private SharingInstanceRepository sharingInstanceRepository;
  @MockitoBean
  private ConsortiumRepository consortiumRepository;
  @MockitoBean
  private TenantRepository tenantRepository;
  @MockitoBean
  private ConsortiaConfigurationService configurationService;

  /* Success cases */
  @Test
  void shouldGetSharingInstanceByActionId() throws Exception {
    var headers = defaultHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);
    SharingInstance sharingInstance = createSharingInstance(ACTION_ID, INSTANCE_ID, "college", "mobius");

    when(configurationService.getCentralTenantId(any())).thenReturn(TENANT);
    when(sharingInstanceService.getById(any(), any())).thenReturn(sharingInstance);

    this.mockMvc.perform(
      get("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/sharing/instances/dcfc317b-0d7c-4334-8656-596105fa6c99")
        .headers(headers)
        .contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON))
      .andDo(print())
      .andExpect(status().isOk())
      .andExpect(jsonPath("$.id").value(String.valueOf(ACTION_ID)));
  }

  @ParameterizedTest
  @ValueSource(strings = {
    "{\"instanceIdentifier\":\"111841e3-e6fb-4191-8fd8-5674a5107c33\",\"sourceTenantId\":\"college\", \"targetTenantId\":\"mobius\"}"
  })
  void shouldSaveSharingInstance(String body) throws Exception {
    SharingInstance sharingInstance = createSharingInstance(INSTANCE_ID, "college", "mobius");
    var headers = defaultHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);

    when(configurationService.getCentralTenantId(any())).thenReturn(TENANT);
    when(sharingInstanceService.start(any(), any())).thenReturn(sharingInstance);

    this.mockMvc.perform(
      post("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/sharing/instances")
        .headers(headers)
        .content(body)
        .contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON))
      .andDo(print())
      .andExpect(status().isCreated())
      .andExpect(jsonPath("$.instanceIdentifier").value(String.valueOf(INSTANCE_ID)))
      .andExpect(jsonPath("$.sourceTenantId").value("college"))
      .andExpect(jsonPath("$.targetTenantId").value("mobius"));
  }

  /* Error cases */
  @Test
  void shouldThrowNotFoundExceptionWhenGettingSharingInstanceByActionId() throws Exception {
    var headers = defaultHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);
    UUID actionId = UUID.fromString("3be3f9da-9d80-402c-905e-e5fa104da3e1");

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);
    when(tenantRepository.existsById(any())).thenReturn(true);
    when(sharingInstanceRepository.findById(actionId)).thenReturn(null);

    this.mockMvc.perform(
        get("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/sharing/instance/3be3f9da-9d80-402c-905e-e5fa104da3e1")
          .headers(headers)
          .contentType(MediaType.APPLICATION_JSON)
          .accept(MediaType.APPLICATION_JSON))
      .andDo(print())
      .andExpect(status().isNotFound());
  }
}
