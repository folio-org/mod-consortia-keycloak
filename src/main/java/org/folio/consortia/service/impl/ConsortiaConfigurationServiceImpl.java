package org.folio.consortia.service.impl;

import java.util.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.consortia.domain.dto.ConsortiaConfiguration;
import org.folio.consortia.domain.entity.ConsortiaConfigurationEntity;
import org.folio.consortia.exception.ResourceAlreadyExistException;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.ConsortiaConfigurationRepository;
import org.folio.consortia.service.ConsortiaConfigurationService;
import org.folio.consortia.utils.TenantContextUtils;
import org.folio.spring.FolioExecutionContext;
import org.springframework.core.convert.ConversionService;
import org.springframework.stereotype.Service;

@Log4j2
@Service
@RequiredArgsConstructor
public class ConsortiaConfigurationServiceImpl implements ConsortiaConfigurationService {
  private static final String CONSORTIA_CONFIGURATION_EXIST_MSG_TEMPLATE =
    "System can not have more than one configuration record";
  private final ConsortiaConfigurationRepository configurationRepository;
  private final ConversionService converter;
  private final FolioExecutionContext folioExecutionContext;

  @Override
  public ConsortiaConfiguration getConsortiaConfiguration() {
    String requestedTenantId = TenantContextUtils.getTenantIdFromHeader(folioExecutionContext);
    ConsortiaConfigurationEntity configuration = getConfiguration(requestedTenantId);
    return converter.convert(configuration, ConsortiaConfiguration.class);
  }

  @Override
  public String getCentralTenantId(String requestTenantId) {
    return getConfiguration(requestTenantId).getCentralTenantId();
  }

  @Override
  public ConsortiaConfiguration createConfiguration(String centralTenantId) {
    checkAnyConsortiaConfigurationNotExistsOrThrow();
    ConsortiaConfigurationEntity configuration = new ConsortiaConfigurationEntity();
    configuration.setCentralTenantId(centralTenantId);
    return converter.convert(configurationRepository.save(configuration), ConsortiaConfiguration.class);
  }

  private ConsortiaConfigurationEntity getConfiguration(String requestTenantId) {
    List<ConsortiaConfigurationEntity> configList = configurationRepository.findAll();
    if (configList.isEmpty()) {
      throw new ResourceNotFoundException("A central tenant not found in this tenant '{}' configuration", requestTenantId);
    }
    log.info("getConfiguration:: configuration with centralTenantId={} is retrieved", configList.get(0).getCentralTenantId());
    return configList.get(0);
  }

  public boolean isCentralTenantConfigurationExists() {
    return configurationRepository.count() > 0;
  }

  private void checkAnyConsortiaConfigurationNotExistsOrThrow() {
    if (configurationRepository.count() > 0) {
      throw new ResourceAlreadyExistException(CONSORTIA_CONFIGURATION_EXIST_MSG_TEMPLATE);
    }
  }

}
