package org.folio.consortia.client;

import org.folio.consortia.domain.dto.ConsortiaConfiguration;
import org.folio.spring.config.FeignClientConfiguration;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

@FeignClient(name = "consortia-configuration" , configuration = FeignClientConfiguration.class)
public interface ConsortiaConfigurationClient {

  @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
  void saveConfiguration(@RequestBody ConsortiaConfiguration configuration);

}
