package org.folio.consortia.base;

import static org.folio.consortia.support.EntityUtils.getFolioExecutionContext;
import static org.folio.spring.integration.XOkapiHeaders.TENANT;
import static org.folio.spring.integration.XOkapiHeaders.TOKEN;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.folio.spring.FolioExecutionContext;
import org.folio.spring.integration.XOkapiHeaders;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.function.Executable;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.batch.autoconfigure.BatchAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.bean.override.mockito.MockitoBean;

@SpringBootTest
@EnableAutoConfiguration(exclude = BatchAutoConfiguration.class)
public abstract class BaseUnitTest {

  protected static final String USER_ID = UUID.randomUUID().toString();
  protected static final String CENTRAL_TENANT_NAME = "central";

  @MockitoBean
  protected FolioExecutionContext folioExecutionContext;

  @BeforeEach
  void before (){
    when(folioExecutionContext.getTenantId()).thenReturn(CENTRAL_TENANT_NAME);
    when(folioExecutionContext.getOkapiHeaders()).thenReturn(defaultHeaders());
    when(folioExecutionContext.getAllHeaders()).thenReturn(defaultHeaders());
    when(folioExecutionContext.getFolioModuleMetadata()).thenReturn(getFolioExecutionContext().getFolioModuleMetadata());
  }

  protected Map<String, Collection<String>> defaultHeaders() {
    Map<String, Collection<String>> map = new HashMap<>();
    map.put(TENANT, List.of(TENANT));
    map.put(TOKEN, List.of(TOKEN));
    map.put(XOkapiHeaders.USER_ID, List.of(USER_ID));
    return map;
  }

  public void assertThrowsCause(Class<?> clazz, Executable executable) {
    assertThat(assertThrows(Throwable.class, executable).getCause(), is(instanceOf(clazz)));
  }

}
