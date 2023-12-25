package org.folio.consortia.service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;

import org.folio.consortia.domain.dto.PublicationHttpResponse;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.integration.XOkapiHeaders;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import com.fasterxml.jackson.databind.ObjectMapper;

import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class HttpRequestServiceImpl implements HttpRequestService {
  private final RestTemplate restTemplate;
  private final FolioExecutionContext folioExecutionContext;
  private final ObjectMapper objectMapper;

  @SneakyThrows
  @Override
  public PublicationHttpResponse performRequest(String url, HttpMethod httpMethod, Object payload) {
    var headers = convertHeadersToMultiMap(folioExecutionContext.getOkapiHeaders());
    headers.setAccept(Collections.singletonList(MediaType.ALL));
    headers.setContentType(MediaType.APPLICATION_JSON);

    HttpEntity<Object> httpEntity = new HttpEntity<>(payload, headers);
    var absUrl = folioExecutionContext.getOkapiUrl() + url;
    log.debug("performRequest:: folio context header TENANT = {}", folioExecutionContext.getOkapiHeaders().get(XOkapiHeaders.TENANT).iterator().next());

    var responseEntity = switch (httpMethod.toString()) {
      case "GET", "POST", "PUT" -> restTemplate.exchange(absUrl, httpMethod, httpEntity, Object.class);
      case "DELETE" -> restTemplate.exchange(absUrl, httpMethod, httpEntity, String.class);
      default -> throw new IllegalStateException("Unexpected HTTP method value: " + httpMethod);
    };

    return new PublicationHttpResponse(objectMapper.writeValueAsString(responseEntity.getBody()), responseEntity.getStatusCode());
  }

  private HttpHeaders convertHeadersToMultiMap(Map<String, Collection<String>> contextHeaders) {
    HttpHeaders multimapHeaders = new HttpHeaders();
    contextHeaders.forEach((key, value) -> multimapHeaders.put(key, new ArrayList<>(value)));

    return multimapHeaders;
  }

}
