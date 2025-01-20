package org.folio.consortia.service;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.net.URI;
import java.util.List;

import org.folio.consortia.client.EurekaProxyTenantsClient;
import org.folio.consortia.domain.dto.ModuleForTenant;
import org.folio.consortia.service.impl.ModuleTenantServiceImpl;
import org.folio.spring.FolioExecutionContext;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;


@ExtendWith(MockitoExtension.class)
class ModuleTenantServiceTest {

  @Mock
  private FolioExecutionContext folioExecutionContext;
  @Mock
  private EurekaProxyTenantsClient eurekaProxyTenantsClient;

  @InjectMocks
  private ModuleTenantServiceImpl moduleTenantService;

  @Test
  void getModuleIdForEurekaTest() {
    var tenantId = "diku";
    var moduleId = "mod-users-19.4.1-SNAPSHOT.322";

    var module1 = new ModuleForTenant();
    module1.setId(moduleId);
    var module2 = new ModuleForTenant();
    module2.setId("mod-users-bl-7.9.2-SNAPSHOT.170");
    var module3 = new ModuleForTenant();
    module3.setId("mod-users");

    when(folioExecutionContext.getTenantId()).thenReturn(tenantId);
    when(eurekaProxyTenantsClient.getModules(isA(URI.class), eq(tenantId))).thenReturn(List.of(module1, module2, module3));

    var actual = moduleTenantService.getModUsersModuleId();

    verify(eurekaProxyTenantsClient).getModules(isA(URI.class), eq(tenantId));
    assertEquals(moduleId, actual);
  }
}
