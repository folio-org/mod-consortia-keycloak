package org.folio.consortia.service.impl;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import org.folio.consortia.domain.dto.TenantDetails.SetupStatusEnum;
import org.folio.consortia.repository.TenantDetailsRepository;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.integration.XOkapiHeaders;
import org.folio.consortia.utils.TenantContextUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class TenantServiceImplTest {

  private static final String TENANT_ID = "member";
  private static final String CENTRAL_TENANT_ID = "central";

  @Mock
  private TenantDetailsRepository tenantDetailsRepository;
  @Mock
  private FolioExecutionContext folioExecutionContext;
  @Mock
  private FolioModuleMetadata folioModuleMetadata;
  @InjectMocks
  private TenantServiceImpl tenantService;

  @BeforeEach
  void setUp() {
    Map<String, Collection<String>> headers = Map.of(XOkapiHeaders.TENANT, List.of(TENANT_ID));
    when(folioExecutionContext.getOkapiHeaders()).thenReturn(headers);
    when(folioExecutionContext.getAllHeaders()).thenReturn(headers);
    when(folioExecutionContext.getFolioModuleMetadata()).thenReturn(folioModuleMetadata);
  }

  @Test
  void updateTenantSetupStatus_updatesStatusInCentralTenantContext() {
    when(tenantDetailsRepository.setSetupStatusByTenantId(SetupStatusEnum.COMPLETED, TENANT_ID)).thenReturn(1);

    try (var contextUtils = mockStatic(TenantContextUtils.class, CALLS_REAL_METHODS)) {
      tenantService.updateTenantSetupStatus(TENANT_ID, CENTRAL_TENANT_ID, SetupStatusEnum.COMPLETED);

      // the update must be executed in the central tenant context, as tenant details are stored in the central tenant
      contextUtils.verify(() -> TenantContextUtils.prepareContextForTenant(eq(CENTRAL_TENANT_ID), any(), any()));
    }

    verify(tenantDetailsRepository).setSetupStatusByTenantId(SetupStatusEnum.COMPLETED, TENANT_ID);
  }

  @Test
  void updateTenantSetupStatus_tenantNotFound() {
    when(tenantDetailsRepository.setSetupStatusByTenantId(SetupStatusEnum.FAILED, TENANT_ID)).thenReturn(0);

    tenantService.updateTenantSetupStatus(TENANT_ID, CENTRAL_TENANT_ID, SetupStatusEnum.FAILED);

    verify(tenantDetailsRepository).setSetupStatusByTenantId(SetupStatusEnum.FAILED, TENANT_ID);
  }
}
