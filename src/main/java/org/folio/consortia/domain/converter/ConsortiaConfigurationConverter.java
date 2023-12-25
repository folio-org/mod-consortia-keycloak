package org.folio.consortia.domain.converter;

import org.folio.consortia.domain.entity.ConsortiaConfigurationEntity;
import org.folio.consortia.domain.dto.ConsortiaConfiguration;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
public class ConsortiaConfigurationConverter implements Converter<ConsortiaConfigurationEntity, ConsortiaConfiguration> {

  @Override
  public ConsortiaConfiguration convert(ConsortiaConfigurationEntity source) {
    ConsortiaConfiguration configuration = new ConsortiaConfiguration();
    configuration.setId(source.getId());
    configuration.setCentralTenantId(source.getCentralTenantId());
    return configuration;
  }
}
