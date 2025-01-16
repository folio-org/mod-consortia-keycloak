package org.folio.consortia.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.ObjectUtils;
import org.folio.consortia.config.kafka.KafkaService;
import org.folio.consortia.domain.dto.PrimaryAffiliationEvent;
import org.folio.consortia.domain.dto.PublicationHttpResponse;
import org.folio.consortia.domain.dto.UserTenant;
import org.folio.consortia.domain.entity.TenantEntity;
import org.folio.consortia.service.HttpRequestService;
import org.folio.consortia.service.PrimaryAffiliationService;
import org.folio.consortia.service.UserTenantService;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.integration.XOkapiHeaders;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestTemplate;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.UUID;

@Service
@Log4j2
@RequiredArgsConstructor
public class PrimaryAffiliationServiceImpl implements PrimaryAffiliationService {

  private final UserTenantService userTenantService;
  private final KafkaService kafkaService;
  private final ObjectMapper objectMapper = new ObjectMapper();

  @Override
  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public void createPrimaryAffiliationInNewTransaction(UUID consortiumId,
                                                       String centralTenantId,
                                                       TenantEntity tenantEntity,
                                                       PrimaryAffiliationEvent event) {
    createAndSendEvent(consortiumId, centralTenantId, tenantEntity, event);
  }

  @Override
  @Transactional
  public void createPrimaryAffiliation(UUID consortiumId,
                                       String centralTenantId,
                                       TenantEntity tenantEntity,
                                       PrimaryAffiliationEvent event) {
    createAndSendEvent(consortiumId, centralTenantId, tenantEntity, event);
  }

  @SneakyThrows
  private void createAndSendEvent(UUID consortiumId,
                                  String centralTenantId,
                                  TenantEntity tenantEntity,
                                  PrimaryAffiliationEvent event) {
    userTenantService.createPrimaryUserTenantAffiliation(consortiumId, tenantEntity, event.getUserId().toString(), event.getUsername());
    if (ObjectUtils.notEqual(centralTenantId, tenantEntity.getId())) {
      userTenantService.save(consortiumId, createUserTenant(centralTenantId, event.getUserId(), event.getUsername()), true);
    }
    String data = objectMapper.writeValueAsString(event);
    kafkaService.send(KafkaService.Topic.CONSORTIUM_PRIMARY_AFFILIATION_CREATED, event.getUserId().toString(), data);
    log.info("Primary affiliation has been created and event sent for the user: {}", event.getUserId());
  }

  private UserTenant createUserTenant(String tenantId, UUID userId, String username) {
    UserTenant userTenant = new UserTenant();
    userTenant.setTenantId(tenantId);
    userTenant.setUserId(userId);
    userTenant.setUsername(username);
    return userTenant;
  }

  @Service
  @RequiredArgsConstructor
  @Log4j2
  public static class HttpRequestServiceImpl implements HttpRequestService {
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
}
