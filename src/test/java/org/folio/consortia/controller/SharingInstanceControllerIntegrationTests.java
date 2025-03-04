package org.folio.consortia.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.folio.consortia.repository.ConsortiumRepository;
import org.folio.consortia.repository.TenantRepository;
import org.folio.consortia.service.ConsortiaConfigurationService;
import org.folio.consortia.service.TenantService;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.folio.consortia.base.BaseIT;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.ArgumentsProvider;
import org.junit.jupiter.params.provider.ArgumentsSource;
import org.springframework.http.MediaType;
import org.springframework.test.context.bean.override.mockito.MockitoBean;

import lombok.SneakyThrows;
import wiremock.net.minidev.json.JSONObject;

class SharingInstanceControllerIntegrationTests extends BaseIT {

  private static boolean INITIALIZED = false;
  private static final String UNIVERSITY = "university";
  private static final String COLLEGE = "college";
  private static final String STATUS = "IN_PROGRESS";

  private static final String[][] SHARING_INSTANCES = {
    {"8673c2b0-dfe6-447b-bb6e-a1d7eb2e3572", TENANT, UNIVERSITY, STATUS},
    {"8673c2b0-dfe6-447b-bb6e-a1d7eb2e3572", TENANT, COLLEGE, STATUS},
    {"d5649ef9-231d-4293-8657-c86b01d46ccc", TENANT, UNIVERSITY, STATUS},
    {"1eb8fc73-24aa-424e-9487-24c178313783", UNIVERSITY, TENANT, STATUS},
    {"ac9865a8-8e17-4351-adb6-eb0a18cdcf9b", UNIVERSITY, TENANT, STATUS},
    {"c3291fa4-b7f0-40c9-ab93-68eec638d9eb", COLLEGE, TENANT, STATUS}
  };

  @MockitoBean
  private ConsortiumRepository consortiumRepository;
  @MockitoBean
  private TenantRepository tenantRepository;
  @MockitoBean
  private ConsortiaConfigurationService configurationService;
  @MockitoBean
  private TenantService tenantService;

  @BeforeEach
  void initialize() {
    if (!INITIALIZED) {
      postAndVerifyResponseBody(SHARING_INSTANCES);
      INITIALIZED = true;
    }
  }

  @ParameterizedTest
  @ArgumentsSource(ParameterArgumentsProvider.class)
  void canGetSharingInstancesByFiltering(String instanceId, String sourceTenantId, String targetTenantId, String status, String totalRecords) {
    // prepare request params
    String params = params(instanceId, sourceTenantId, targetTenantId, status);

    getAndVerifyTotalRecords(params, totalRecords);
  }

  @SneakyThrows
  private void getAndVerifyTotalRecords(String params, String totalRecords) {
    // to skip validation for existence of consortium
    when(consortiumRepository.existsById(any())).thenReturn(true);
    when(configurationService.getCentralTenantId(any())).thenReturn(TENANT);

    this.mockMvc.perform(
        get("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/sharing/instances" + params)
          .headers(defaultHeaders())
          .contentType(MediaType.APPLICATION_JSON)
          .accept(MediaType.APPLICATION_JSON))
      .andExpect(jsonPath("$.totalRecords").value(totalRecords));
  }

  @SneakyThrows
  private void postAndVerifyResponseBody(String[][] instances) {
    for (String[] fields : instances) {
      // to skip the validation part
      when(configurationService.getCentralTenantId(any())).thenReturn(TENANT);
      when(consortiumRepository.existsById(any())).thenReturn(true);
      when(tenantRepository.existsById(any())).thenReturn(true);
      when(tenantService.getCentralTenantId()).thenReturn(fields[2]);

      // create payload
      String body = new JSONObject()
        .appendField("instanceIdentifier", fields[0])
        .appendField("sourceTenantId", fields[1])
        .appendField("targetTenantId", fields[2])
        .appendField("status", fields[3])
        .toJSONString();

      // POST and verify response
      this.mockMvc.perform(
          post("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/sharing/instances")
            .headers(defaultHeaders())
            .content(body)
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isCreated())
        .andExpect(jsonPath("$.id").isNotEmpty())
        .andExpect(jsonPath("$.instanceIdentifier").value(fields[0]))
        .andExpect(jsonPath("$.sourceTenantId").value(fields[1]))
        .andExpect(jsonPath("$.targetTenantId").value(fields[2]))
        .andExpect(jsonPath("$.status").value(fields[3]));
    }
  }

  private String params(String instanceId, String sourceTenantId, String targetTenantId, String status) {
    Map<String, String> parameters = new HashMap<>();
    parameters.put("instanceIdentifier", instanceId);
    parameters.put("sourceTenantId", sourceTenantId);
    parameters.put("targetTenantId", targetTenantId);
    parameters.put("status", status);

    String query = parameters.keySet()
      .stream().filter(k -> Objects.nonNull(parameters.get(k)))
      .map(k -> k + "=" + parameters.get(k))
      .collect(Collectors.joining("&"));

    return query.length() == 0 ? "" : "?" + query;
  }

  private static class ParameterArgumentsProvider implements ArgumentsProvider {
    @Override
    public Stream<? extends Arguments> provideArguments(ExtensionContext context) {
      return Stream.of(
        Arguments.of("c3291fa4-b7f0-40c9-ab93-68eec638d9eb", COLLEGE, TENANT, STATUS, "1"),
        Arguments.of("ac9865a8-8e17-4351-adb6-eb0a18cdcf9b", COLLEGE, UNIVERSITY, null, "0"),
        Arguments.of("8673c2b0-dfe6-447b-bb6e-a1d7eb2e3572", TENANT, null, STATUS, "2"),
        Arguments.of("8673c2b0-dfe6-447b-bb6e-a1d7eb2e3572", TENANT, null, null, "2"),
        Arguments.of("d5649ef9-231d-4293-8657-c86b01d46ccc", null, UNIVERSITY, STATUS, "1"),
        Arguments.of("ac9865a8-8e17-4351-adb6-eb0a18cdcf9b", null, TENANT, null, "1"),
        Arguments.of("8673c2b0-dfe6-447b-bb6e-a1d7eb2e3572", null, null, STATUS, "2"),
        Arguments.of("1eb8fc73-24aa-424e-9487-24c178313783", null, null, null, "1"),
        Arguments.of(null, TENANT, UNIVERSITY, STATUS, "2"),
        Arguments.of(null, TENANT, UNIVERSITY, null, "2"),
        Arguments.of(null, COLLEGE, null, STATUS, "1"),
        Arguments.of(null, TENANT, null, null, "3"),
        Arguments.of(null, null, UNIVERSITY, STATUS, "2"),
        Arguments.of(null, null, UNIVERSITY, null, "2"),
        Arguments.of(null, null, null, STATUS, "6"),
        Arguments.of(null, null, null, null, "6"),
        Arguments.of(null, "", "", null, "6")
      );
    }
  }
}
