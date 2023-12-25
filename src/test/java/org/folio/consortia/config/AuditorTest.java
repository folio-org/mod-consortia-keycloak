package org.folio.consortia.config;

import org.folio.spring.FolioExecutionContext;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.batch.BatchAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.UUID;

@SpringBootTest
@EnableAutoConfiguration(exclude = BatchAutoConfiguration.class)
public class AuditorTest {
  @Mock
  FolioExecutionContext folioExecutionContext;
  @InjectMocks
  FolioAuditorAware folioAuditorAware;

  @Test
  void shouldGetCurrentUserId() {
    Mockito.when(folioExecutionContext.getUserId()).thenReturn(UUID.randomUUID());
    Assertions.assertNotNull(folioAuditorAware.getCurrentAuditor());
  }
}
