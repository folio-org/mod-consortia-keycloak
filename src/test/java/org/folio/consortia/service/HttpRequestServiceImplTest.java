package org.folio.consortia.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import org.apache.commons.lang3.RandomStringUtils;
import org.folio.consortia.base.BaseUnitTest;
import org.folio.consortia.service.impl.HttpRequestServiceImpl;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.web.client.RestTemplate;

import tools.jackson.databind.json.JsonMapper;


class HttpRequestServiceImplTest extends BaseUnitTest {

  @Autowired
  HttpRequestServiceImpl httpRequestService;
  @MockitoBean
  RestTemplate restTemplate;
  @MockitoBean
  JsonMapper objectMapper;

  @Test
  void performRequestSuccess() {
    String payload = RandomStringUtils.random(10);

    ResponseEntity<Object> restTemplateResponse = new ResponseEntity<>(payload, HttpStatusCode.valueOf(201));
    when(folioExecutionContext.getTenantId()).thenReturn(CENTRAL_TENANT_NAME);
    when(folioExecutionContext.getOkapiHeaders()).thenReturn(defaultHeaders());
    when(folioExecutionContext.getAllHeaders()).thenReturn(defaultHeaders());
    when(restTemplate.exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), eq(Object.class))).thenReturn(restTemplateResponse);
    when(objectMapper.writeValueAsString(any())).thenReturn(payload);

    var response = httpRequestService.performRequest(RandomStringUtils.random(10), HttpMethod.POST, new Object());
    Assertions.assertEquals(payload, response.getBody());
  }
}
