package org.folio.consortia.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

import feign.Request;
import org.folio.consortia.domain.dto.PublicationRequest;
import org.folio.consortia.domain.dto.PublicationResponse;
import org.folio.consortia.repository.ConsortiumRepository;
import org.folio.consortia.repository.PublicationStatusRepository;
import org.folio.spring.DefaultFolioExecutionContext;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.integration.XOkapiHeaders;
import org.folio.spring.scope.FolioExecutionContextSetter;
import org.folio.spring.service.SystemUserScopedExecutionService;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.Mock;
import org.mockito.invocation.InvocationOnMock;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.task.TaskExecutor;
import org.springframework.test.util.ReflectionTestUtils;

import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.Callable;

import static org.folio.consortia.support.EntityUtils.CENTRAL_TENANT_ID;
import static org.folio.consortia.support.EntityUtils.TENANT_ID_1;
import static org.folio.consortia.support.EntityUtils.TENANT_ID_2;
import static org.folio.consortia.support.EntityUtils.createOkapiHeaders;
import static org.folio.consortia.support.EntityUtils.createTenant;
import static org.folio.consortia.support.EntityUtils.createTenantCollection;
import static org.folio.consortia.support.TestConstants.CONSORTIUM_ID;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@SpringBootTest
public abstract class BaseSharingConfigServiceTest {

  @Mock
  protected ConsortiumRepository consortiumRepository;
  @Mock
  protected ConsortiumService consortiumService;
  @Mock
  protected TaskExecutor asyncTaskExecutor;
  @Mock
  protected TenantService tenantService;
  @Mock
  protected PublicationService publicationService;
  @Mock
  protected PublicationStatusRepository publicationStatusRepository;
  @Mock
  protected FolioExecutionContext folioExecutionContext;
  @Mock
  protected SystemUserScopedExecutionService systemUserScopedExecutionService;
  @Mock
  protected ObjectMapper objectMapper;

  @BeforeEach
  void setUp() {
    var tenant1 = createTenant(TENANT_ID_1);
    var tenant2 = createTenant(TENANT_ID_2);
    var tenantCollection = createTenantCollection(List.of(tenant1, tenant2));

    when(consortiumRepository.existsById(CONSORTIUM_ID)).thenReturn(true);
    when(folioExecutionContext.getTenantId()).thenReturn(CENTRAL_TENANT_ID);
    when(systemUserScopedExecutionService.executeSystemUserScoped(eq(CENTRAL_TENANT_ID), any()))
      .then(this::callSecondArgument);
    when(tenantService.getAll(CONSORTIUM_ID)).thenReturn(tenantCollection);
    Map<String, Collection<String>> okapiHeaders = createOkapiHeaders();
    when(folioExecutionContext.getOkapiHeaders()).thenReturn(okapiHeaders);

    ReflectionTestUtils.setField(getServiceUnderTest(), "maxTries", 60);
    ReflectionTestUtils.setField(getServiceUnderTest(), "interval", 200);
  }

  protected void setupCommonMocksForStart(UUID createPcId, UUID updatePcId, PublicationRequest expectedPubRequestPost,
                                          PublicationRequest expectedPubRequestPut, ObjectNode payload) {
    when(publicationService.publishRequest(CONSORTIUM_ID, expectedPubRequestPost))
      .thenReturn(new PublicationResponse().id(createPcId));
    when(publicationService.publishRequest(CONSORTIUM_ID, expectedPubRequestPut))
      .thenReturn(new PublicationResponse().id(updatePcId));
    when(objectMapper.convertValue(any(), eq(ObjectNode.class))).thenReturn(payload);
  }

  protected void setupCommonMocksForDelete(UUID pcId, PublicationRequest publicationRequestDelete) {
    when(publicationService.publishRequest(CONSORTIUM_ID, publicationRequestDelete))
      .thenReturn(new PublicationResponse().id(pcId));
  }

  protected <T> T callSecondArgument(InvocationOnMock invocation) throws Exception {
    var headers = Map.<String, Collection<String>>of(XOkapiHeaders.TENANT, List.of(CENTRAL_TENANT_ID));
    var context = new DefaultFolioExecutionContext(mock(FolioModuleMetadata.class), headers);
    try (var ignored = new FolioExecutionContextSetter(context)) {
      return invocation.<Callable<T>>getArgument(1).call();
    }
  }

  protected Request buildFeignRequest() {
    return Request
      .create(Request.HttpMethod.GET,
        "/roles",
        Collections.emptyMap(),
        null,
        StandardCharsets.UTF_8,
        null);
  }

  protected abstract Object getServiceUnderTest();
}
