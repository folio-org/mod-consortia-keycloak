package org.folio.consortia.config.keycloak;

import feign.Client;
import lombok.extern.log4j.Log4j2;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.folio.common.utils.tls.FeignClientTlsUtils;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.security.Security;

@Log4j2
@Configuration
public class KeycloakFeignClientConfig {

  public static final String BCFKS_KEYSTORE_TYPE = "BCFKS";

  static {
    Security.addProvider(new BouncyCastleProvider());
  }

  @Bean
  public Client feignClient(KeycloakProperties properties, okhttp3.OkHttpClient okHttpClient) {
    return FeignClientTlsUtils.getOkHttpClient(okHttpClient, properties.getTls());
  }
}
