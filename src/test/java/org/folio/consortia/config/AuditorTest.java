package org.folio.consortia.config;

import org.folio.spring.FolioExecutionContext;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.batch.autoconfigure.BatchAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.bean.override.mockito.MockitoBean;

import java.util.UUID;

@SpringBootTest
@EnableAutoConfiguration(exclude = BatchAutoConfiguration.class)
public class AuditorTest {
  @MockitoBean
  FolioExecutionContext folioExecutionContext;
  @Autowired
  FolioAuditorAware folioAuditorAware;

  @Test
  void shouldGetCurrentUserId() {
    Mockito.when(folioExecutionContext.getUserId()).thenReturn(UUID.randomUUID());
    Assertions.assertNotNull(folioAuditorAware.getCurrentAuditor());
  }
}
