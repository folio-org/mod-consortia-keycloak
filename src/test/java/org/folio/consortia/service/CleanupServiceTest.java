package org.folio.consortia.service;

import static org.mockito.Mockito.*;

import org.folio.consortia.domain.entity.ConsortiumEntity;
import org.folio.consortia.repository.*;
import org.folio.consortia.support.CopilotGenerated;
import org.folio.spring.FolioExecutionContext;
import org.folio.consortia.service.impl.CleanupServiceImpl;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDateTime;
import java.util.Collections;

@CopilotGenerated
@ExtendWith(MockitoExtension.class)
class CleanupServiceTest {

  @Mock
  private FolioExecutionContext folioExecutionContext;
  @Mock
  private ConsortiumRepository consortiumRepository;
  @Mock
  private PublicationStatusRepository publicationStatusRepository;
  @Mock
  private PublicationTenantRequestRepository publicationTenantRequestRepository;
  @Mock
  private SharingInstanceRepository sharingInstanceRepository;
  @Mock
  private SharingRoleRepository sharingRoleRepository;
  @Mock
  private SharingPolicyRepository sharingPolicyRepository;
  @Mock
  private SharingSettingRepository sharingSettingRepository;

  @InjectMocks
  private CleanupServiceImpl cleanupService;

  @Test
  void clearPublicationTables_removesRecordsSuccessfully() {
    when(folioExecutionContext.getTenantId()).thenReturn("tenant1");
    when(consortiumRepository.findAll()).thenReturn(Collections.singletonList(new ConsortiumEntity()));
    when(publicationTenantRequestRepository.deleteAllByCreatedDateBefore(any(LocalDateTime.class))).thenReturn(5);
    when(publicationStatusRepository.deleteAllByCreatedDateBefore(any(LocalDateTime.class))).thenReturn(3);

    cleanupService.clearPublicationTables();

    verify(publicationTenantRequestRepository).deleteAllByCreatedDateBefore(any(LocalDateTime.class));
    verify(publicationStatusRepository).deleteAllByCreatedDateBefore(any(LocalDateTime.class));
  }

  @Test
  void clearPublicationTables_noConsortiumRecords() {
    when(folioExecutionContext.getTenantId()).thenReturn("tenant1");
    when(consortiumRepository.findAll()).thenReturn(Collections.emptyList());

    cleanupService.clearPublicationTables();

    verify(publicationTenantRequestRepository, never()).deleteAllByCreatedDateBefore(any(LocalDateTime.class));
    verify(publicationStatusRepository, never()).deleteAllByCreatedDateBefore(any(LocalDateTime.class));
  }

  @Test
  void clearSharingTables_removesRecordsSuccessfully() {
    String tenantId = "tenant1";
    when(consortiumRepository.findAll()).thenReturn(Collections.singletonList(new ConsortiumEntity()));
    when(sharingInstanceRepository.deleteInstancesForTenant(tenantId)).thenReturn(4);
    when(sharingRoleRepository.deleteRolesForTenant(tenantId)).thenReturn(2);
    when(sharingPolicyRepository.deletePoliciesForTenant(tenantId)).thenReturn(3);
    when(sharingSettingRepository.deleteRolesForTenant(tenantId)).thenReturn(1);

    cleanupService.clearSharingTables(tenantId);

    verify(sharingInstanceRepository).deleteInstancesForTenant(tenantId);
    verify(sharingRoleRepository).deleteRolesForTenant(tenantId);
    verify(sharingPolicyRepository).deletePoliciesForTenant(tenantId);
    verify(sharingSettingRepository).deleteRolesForTenant(tenantId);
  }

  @Test
  void clearSharingTables_noConsortiumRecords() {
    String tenantId = "tenant1";
    when(consortiumRepository.findAll()).thenReturn(Collections.emptyList());

    cleanupService.clearSharingTables(tenantId);

    verify(sharingInstanceRepository, never()).deleteInstancesForTenant(tenantId);
    verify(sharingRoleRepository, never()).deleteRolesForTenant(tenantId);
    verify(sharingPolicyRepository, never()).deletePoliciesForTenant(tenantId);
    verify(sharingSettingRepository, never()).deleteRolesForTenant(tenantId);
  }
}
