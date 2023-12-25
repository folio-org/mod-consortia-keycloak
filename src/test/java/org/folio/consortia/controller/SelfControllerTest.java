package org.folio.consortia.controller;

import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.http.MediaType.APPLICATION_JSON;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.folio.consortia.repository.ConsortiumRepository;
import org.folio.consortia.service.UserTenantService;
import java.util.List;
import java.util.UUID;

import org.folio.consortia.domain.dto.UserTenant;
import org.folio.consortia.domain.dto.UserTenantCollection;
import org.folio.consortia.support.BaseIT;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.integration.XOkapiHeaders;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpHeaders;
public class SelfControllerTest extends BaseIT {

  @Autowired
  FolioExecutionContext folioExecutionContext;
  @MockBean
  ConsortiumRepository consortiumRepository;
  @MockBean
  UserTenantService userTenantService;
  private static final String CONSORTIUM_ID = "7698e46-c3e3-11ed-afa1-0242ac120002";

  @Test
  void shouldGetAssociatedAffliations() throws Exception {
    List<UserTenant> userTenantDtos = List.of(new UserTenant(), new UserTenant());
    UserTenantCollection userTenantCollection = new UserTenantCollection();
    userTenantCollection.setUserTenants(userTenantDtos);
    userTenantCollection.setTotalRecords(userTenantDtos.size());

    var headers = defaultHeaders();
    UUID consortiumId = UUID.fromString(CONSORTIUM_ID);
    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    when(userTenantService.getByUserId(any(),any(),any(),any())).thenReturn(userTenantCollection);
    this.mockMvc.perform(
      get("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/_self")
        .headers(headers)
      )
      .andExpectAll(status().isOk())
      .andExpect(jsonPath("$.totalRecords", is(2)));
  }

  @Test
  void shouldGetTokenNotFound() throws Exception {
    List<UserTenant> userTenantDtos = List.of(new UserTenant(), new UserTenant());
    UserTenantCollection userTenantCollection = new UserTenantCollection();
    userTenantCollection.setUserTenants(userTenantDtos);
    userTenantCollection.setTotalRecords(userTenantDtos.size());

    var headers = defaultHeadersWithoutToken();
    UUID consortiumId = UUID.fromString(CONSORTIUM_ID);
    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    when(userTenantService.getByUserId(any(),any(),any(),any())).thenReturn(userTenantCollection);
    this.mockMvc.perform(
      get("/consortia/7698e46-c3e3-11ed-afa1-0242ac120002/_self")
        .headers(headers))
      .andExpectAll(status().is4xxClientError())
      .andExpect(jsonPath("$.errors[0].code", is("UNAUTHORIZED")));
  }

  public static HttpHeaders defaultHeadersWithoutToken() {
    final HttpHeaders httpHeaders = new HttpHeaders();

    httpHeaders.setContentType(APPLICATION_JSON);
    httpHeaders.put(XOkapiHeaders.TENANT, List.of(TENANT));
    httpHeaders.add(XOkapiHeaders.URL, wireMockServer.baseUrl());
    httpHeaders.add(XOkapiHeaders.USER_ID, "7698e46-c3e3-11ed-afa1-0242ac120002");

    return httpHeaders;
  }
}
