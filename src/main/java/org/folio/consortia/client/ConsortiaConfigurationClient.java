package org.folio.consortia.client;

import org.folio.consortia.domain.dto.ConsortiaConfiguration;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.service.annotation.DeleteExchange;
import org.springframework.web.service.annotation.HttpExchange;
import org.springframework.web.service.annotation.PostExchange;

@HttpExchange("consortia-configuration")
public interface ConsortiaConfigurationClient {

  @PostExchange(contentType = MediaType.APPLICATION_JSON_VALUE)
  void saveConfiguration(@RequestBody ConsortiaConfiguration configuration);

  @DeleteExchange
  void deleteConfiguration();
}
