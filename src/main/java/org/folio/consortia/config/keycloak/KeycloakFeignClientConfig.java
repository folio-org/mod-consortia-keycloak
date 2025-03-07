package org.folio.consortia.config.keycloak;

import feign.Client;
import lombok.extern.log4j.Log4j2;
import org.folio.common.utils.tls.FeignClientTlsUtils;
import org.springframework.context.annotation.Bean;

@Log4j2
public class KeycloakFeignClientConfig {
  @Bean
  public Client feignClient(KeycloakProperties properties, okhttp3.OkHttpClient okHttpClient) {
    return FeignClientTlsUtils.getOkHttpClient(okHttpClient, properties.getTls());
  }
}
