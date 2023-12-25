package org.folio.consortia.controller;

import lombok.RequiredArgsConstructor;
import org.folio.consortia.domain.dto.ConsortiaConfiguration;
import org.folio.consortia.rest.resource.ConsortiaConfigurationApi;
import org.folio.consortia.service.ConsortiaConfigurationService;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequiredArgsConstructor
public class ConsortiaConfigurationController implements ConsortiaConfigurationApi {

  private final ConsortiaConfigurationService configurationService;

  @Override
  public ResponseEntity<ConsortiaConfiguration> getConfiguration() {
    return ResponseEntity.ok(configurationService.getConsortiaConfiguration());
  }

  @Override
  public ResponseEntity<ConsortiaConfiguration> saveConfiguration(ConsortiaConfiguration configuration) {
    return ResponseEntity.status(HttpStatus.CREATED)
      .body(configurationService.createConfiguration(configuration.getCentralTenantId()));
  }
}
