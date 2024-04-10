package org.folio.consortia.service;

import org.folio.consortia.domain.entity.ConsortiaConfigurationEntity;
import org.folio.consortia.exception.ResourceAlreadyExistException;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.ConsortiaConfigurationRepository;
import org.folio.consortia.service.impl.ConsortiaConfigurationServiceImpl;
import org.folio.consortia.domain.dto.ConsortiaConfiguration;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.integration.XOkapiHeaders;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.batch.BatchAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.convert.ConversionService;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.folio.consortia.support.EntityUtils.createConsortiaConfiguration;
import static org.folio.consortia.support.EntityUtils.createConsortiaConfigurationEntity;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@SpringBootTest
@EnableAutoConfiguration(exclude = BatchAutoConfiguration.class)
class ConsortiaConfigurationServiceTest {
  private static final String CENTRAL_TENANT_ID = "diku";
  private static final String TENANT_ID = "testtenant1";

  @InjectMocks
  ConsortiaConfigurationServiceImpl configurationService;
  @Mock
  ConsortiaConfigurationRepository configurationRepository;
  @Mock
  FolioExecutionContext folioExecutionContext;
  @Mock
  ConversionService conversionService;

  @Test
  void shouldGetConfigValueByGetCentralTenantId() {
    List<ConsortiaConfigurationEntity> configurationEntityList = List.of(createConsortiaConfigurationEntity(CENTRAL_TENANT_ID));

    when(configurationRepository.findAll()).thenReturn(configurationEntityList);
    String actualCentralTenantId = configurationService.getCentralTenantId(TENANT_ID);

    Assertions.assertEquals(CENTRAL_TENANT_ID, actualCentralTenantId);
  }

  @Test
  void shouldGetConfigValueByGetConsortiaConfiguration() {
    ConsortiaConfigurationEntity configuration = createConsortiaConfigurationEntity(CENTRAL_TENANT_ID);
    List<ConsortiaConfigurationEntity> configurationEntityList = List.of(configuration);

    when(configurationRepository.findAll()).thenReturn(configurationEntityList);
    when(conversionService.convert(configuration, ConsortiaConfiguration.class))
      .thenReturn(createConsortiaConfiguration(CENTRAL_TENANT_ID));
    when(folioExecutionContext.getTenantId()).thenReturn("testtenant1");
    Map<String, Collection<String>> okapiHeaders = new HashMap<>();
    okapiHeaders.put(XOkapiHeaders.TENANT, List.of("testtenant1"));
    when(folioExecutionContext.getOkapiHeaders()).thenReturn(okapiHeaders);


    var expected = configurationService.getConsortiaConfiguration();

    Assertions.assertEquals(CENTRAL_TENANT_ID, expected.getCentralTenantId());
  }

  @Test
  void shouldSaveConfigValue() {
    ConsortiaConfigurationEntity configuration = createConsortiaConfigurationEntity(CENTRAL_TENANT_ID);

    when(configurationRepository.save(any())).thenReturn(configuration);
    when(configurationRepository.count()).thenReturn(0L);

    configurationService.createConfiguration(CENTRAL_TENANT_ID);

    verify(configurationRepository, times(1)).save(any());
  }

  @Test
  void shouldThrowResourceAlreadyExistExceptionErrorWhileSavingConfigValue() {
    ConsortiaConfigurationEntity configuration = createConsortiaConfigurationEntity(CENTRAL_TENANT_ID);

    when(configurationRepository.save(any())).thenReturn(configuration);
    when(configurationRepository.count()).thenReturn(1L);

    Assertions.assertThrows(ResourceAlreadyExistException.class,
      () -> configurationService.createConfiguration(CENTRAL_TENANT_ID));

    verify(configurationRepository, times(0)).save(any());

  }

  @Test
  void shouldThrowCentralTenantNotFoundErrorWhileGetConfigValue() {

    when(configurationRepository.findAll()).thenReturn(new ArrayList<>());

    Assertions.assertThrows(ResourceNotFoundException.class, () -> configurationService.getCentralTenantId(TENANT_ID));
  }

}
