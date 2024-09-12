package org.folio.consortia.controller;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.common.ClasspathFileSource;
import com.github.tomakehurst.wiremock.extension.responsetemplating.ResponseTemplateTransformer;
import com.github.tomakehurst.wiremock.extension.responsetemplating.TemplateEngine;
import java.util.ArrayList;
import org.folio.consortia.base.BaseIT;
import org.folio.consortia.domain.dto.CustomField;
import org.folio.consortia.service.CustomFieldService;
import org.folio.tenant.domain.dto.TenantAttributes;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

public class FolioTenantControllerTest extends BaseIT {

  @MockBean
  CustomFieldService customFieldService;

  @BeforeAll
  static void beforeAll(@Autowired MockMvc mockMvc) {
    //override default tenant setup for integration tests
    wireMockServer = new WireMockServer(wireMockConfig()
      .port(WIRE_MOCK_PORT)
      .extensions(
        new ResponseTemplateTransformer(TemplateEngine.defaultTemplateEngine(), true, new ClasspathFileSource("/"),
          new ArrayList<>())));

    wireMockServer.start();
  }

  @Test
  void enableTenant_negative_customFieldNotCreated() throws Exception {
    when(customFieldService.getCustomFieldByName(anyString())).thenReturn(null);
    doThrow(new RuntimeException("error")).when(customFieldService).createCustomField(any(CustomField.class));

    mockMvc.perform(post("/_/tenant")
        .headers(defaultHeaders())
        .content(asJsonString(new TenantAttributes().moduleTo("mod-consortia-keycloak"))))
      .andExpect(status().is(500));
  }

  @Test
  void enableTenant_positive() throws Exception {
    mockMvc.perform(post("/_/tenant")
        .headers(defaultHeaders())
        .content(asJsonString(new TenantAttributes().moduleTo("mod-consortia-keycloak"))))
      .andExpect(status().is(204));
  }
}
