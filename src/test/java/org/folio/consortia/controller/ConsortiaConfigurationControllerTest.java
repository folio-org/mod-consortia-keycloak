package org.folio.consortia.controller;

import static org.folio.consortia.support.EntityUtils.createConsortiaConfigurationEntity;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.folio.consortia.domain.entity.ConsortiaConfigurationEntity;
import org.folio.consortia.repository.ConsortiaConfigurationRepository;
import java.util.List;

import org.folio.consortia.base.BaseIT;
import org.folio.spring.integration.XOkapiHeaders;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.test.context.bean.override.mockito.MockitoBean;

class ConsortiaConfigurationControllerTest extends BaseIT {

  @MockitoBean
  ConsortiaConfigurationRepository configurationRepository;

  @Test
  void shouldGetConsortiumConfiguration() throws Exception {
    var header = defaultHeaders();
    header.set(XOkapiHeaders.TENANT, "testtenat1");

    when(configurationRepository.findAll())
      .thenReturn(List.of(createConsortiaConfigurationEntity("diku")));

    this.mockMvc.perform(get("/consortia-configuration")
        .headers(header))
      .andExpect(status().isOk())
      .andExpect(jsonPath("$.centralTenantId", is("diku")));
  }

  @ParameterizedTest
  @ValueSource(strings = {"{\"centralTenantId\":\"diku\"}"})
  void shouldSaveConsortiumConfiguration(String contentString) throws Exception {
    var header = defaultHeaders();
    var configuration = createConsortiaConfigurationEntity("diku");

    when(configurationRepository.save(any(ConsortiaConfigurationEntity.class))).thenReturn(configuration);
    when(configurationRepository.findAll()).thenReturn(List.of());

    this.mockMvc.perform(post("/consortia-configuration")
        .headers(header)
        .content(contentString))
      .andExpect(status().isCreated())
      .andExpect(jsonPath("$.centralTenantId", is("diku")));

    verify(configurationRepository).save(any(ConsortiaConfigurationEntity.class));
    verify(configurationRepository).findAll();
  }

  @ParameterizedTest
  @ValueSource(strings = {"{\"centralTenantId\":\"diku\"}"})
  void shouldOverrideConsortiumConfiguration(String contentString) throws Exception {
    var header = defaultHeaders();
    var configuration = createConsortiaConfigurationEntity("diku");

    when(configurationRepository.save(any(ConsortiaConfigurationEntity.class))).thenReturn(configuration);
    when(configurationRepository.findAll()).thenReturn(List.of(configuration));
    doNothing().when(configurationRepository).deleteAll();

    this.mockMvc.perform(post("/consortia-configuration")
        .headers(header)
        .content(contentString))
      .andExpect(status().isCreated())
      .andExpect(jsonPath("$.centralTenantId", is("diku")));

    verify(configurationRepository).save(any(ConsortiaConfigurationEntity.class));
    verify(configurationRepository).findAll();
    verify(configurationRepository).deleteAll();
  }

  @Test
  void shouldDeleteConsortiumConfiguration() throws Exception {
    var header = defaultHeaders();
    var configuration = createConsortiaConfigurationEntity("diku");

    when(configurationRepository.findAll()).thenReturn(List.of(configuration));
    doNothing().when(configurationRepository).deleteAll();

    this.mockMvc.perform(delete("/consortia-configuration")
        .headers(header))
      .andExpect(status().isNoContent());

    verify(configurationRepository).findAll();
    verify(configurationRepository).deleteAll();
  }

  @Test
  void shouldNotDeleteConsortiumConfigurationWhenNotFound() throws Exception {
    var header = defaultHeaders();

    when(configurationRepository.findAll()).thenReturn(List.of());

    this.mockMvc.perform(delete("/consortia-configuration")
        .headers(header))
      .andExpect(status().isNoContent());

    verify(configurationRepository).findAll();
  }

}
