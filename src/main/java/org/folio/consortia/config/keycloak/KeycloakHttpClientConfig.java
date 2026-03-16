package org.folio.consortia.config.keycloak;

import org.folio.common.utils.tls.HttpClientTlsUtils;
import org.folio.consortia.client.KeycloakClient;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.client.EnrichUrlAndHeadersInterceptor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.client.RestClient;

@Configuration
public class KeycloakHttpClientConfig {

  @Bean
  public KeycloakClient keycloakClient(KeycloakProperties properties, RestClient.Builder restClientBuilder, FolioExecutionContext folioExecutionContext) {
    // Override existing interceptor to avoid modifying the request URL for Keycloak endpoints
    var keycloakHttpRequestInterceptor = new EnrichUrlAndHeadersInterceptor(folioExecutionContext) {
      @Override
      protected String prepareUrl(String requestUrl, FolioExecutionContext context) {
        return requestUrl;
      }
    };
    // Build a separate RestClient.Builder clone for Keycloak by replacing the existing EnrichUrlAndHeadersInterceptor with the custom one
    var keycloakRestClientBuilder = restClientBuilder.build()
      .mutate().clone()
      .requestInterceptors(interceptors -> interceptors.removeIf(interceptor -> interceptor instanceof EnrichUrlAndHeadersInterceptor))
      .requestInterceptor(keycloakHttpRequestInterceptor);
    // Configure TLS and build the Keycloak client
    return HttpClientTlsUtils.buildHttpServiceClient(keycloakRestClientBuilder, properties.getTls(), properties.getUrl(), KeycloakClient.class);
  }

}
