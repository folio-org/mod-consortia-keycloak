package org.folio.consortia.config.keycloak;

import org.folio.common.utils.tls.HttpClientTlsUtils;
import org.folio.consortia.client.KeycloakClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.client.RestClient;

@Configuration
public class KeycloakHttpClientConfig {

  @Bean
  public KeycloakClient keycloakClient(KeycloakProperties properties, RestClient.Builder restClientBuilder) {
    return HttpClientTlsUtils.buildHttpServiceClient(restClientBuilder, properties.getTls(), properties.getUrl(), KeycloakClient.class);
  }

}
