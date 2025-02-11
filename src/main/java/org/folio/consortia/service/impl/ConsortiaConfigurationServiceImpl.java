package org.folio.consortia.service.impl;

import java.util.List;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import lombok.val;

import org.folio.consortia.domain.dto.ConsortiaConfiguration;
import org.folio.consortia.domain.entity.ConsortiaConfigurationEntity;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.ConsortiaConfigurationRepository;
import org.folio.consortia.service.ConsortiaConfigurationService;
import org.folio.consortia.utils.TenantContextUtils;
import org.folio.spring.FolioExecutionContext;
import org.springframework.core.convert.ConversionService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Log4j2
@Service
@RequiredArgsConstructor
public class ConsortiaConfigurationServiceImpl implements ConsortiaConfigurationService {
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
  @Transactional
  public ConsortiaConfiguration createConfiguration(String centralTenantId) {
    this.deleteConfiguration();
    log.info("createConfiguration:: Saving new consortia configuration with centralTenantId: '{}'", centralTenantId);
    val configuration = ConsortiaConfigurationEntity.builder().centralTenantId(centralTenantId).build();
    return converter.convert(configurationRepository.save(configuration), ConsortiaConfiguration.class);
  }

  @Override
  public void deleteConfiguration() {
    val existingConfigurations = configurationRepository.findAll();
    if (!existingConfigurations.isEmpty()) {
      log.info("createConfiguration:: Deleting existing configuration with centralTenantId: '{}'", existingConfigurations.get(0).getCentralTenantId());
      configurationRepository.deleteAll();
      configurationRepository.flush();
    } else {
      log.info("createConfiguration:: No existing configuration found to delete");
    }
  }

  private ConsortiaConfigurationEntity getConfiguration(String requestTenantId) {
    List<ConsortiaConfigurationEntity> configList = configurationRepository.findAll();
    if (configList.isEmpty()) {
      throw new ResourceNotFoundException("A central tenant not found in this tenant '{}' configuration", requestTenantId);
    }
    log.info("getConfiguration:: configuration with centralTenantId={} is retrieved", configList.get(0).getCentralTenantId());
    return configList.get(0);
  }

}
