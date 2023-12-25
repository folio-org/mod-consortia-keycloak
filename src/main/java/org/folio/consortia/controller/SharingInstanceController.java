package org.folio.consortia.controller;

import static org.springframework.http.HttpStatus.CREATED;

import java.util.UUID;

import org.folio.consortia.domain.dto.SharingInstance;
import org.folio.consortia.domain.dto.SharingInstanceCollection;
import org.folio.consortia.domain.dto.Status;
import org.folio.consortia.rest.resource.InstancesApi;
import org.folio.consortia.service.ConsortiaConfigurationService;
import org.folio.consortia.service.SharingInstanceService;
import org.folio.consortia.utils.TenantContextUtils;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.scope.FolioExecutionContextSetter;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@RestController
@RequestMapping("/consortia/{consortiumId}/sharing")
@Log4j2
@RequiredArgsConstructor
public class SharingInstanceController implements InstancesApi {

  private final SharingInstanceService sharingInstanceService;
  private final ConsortiaConfigurationService configurationService;
  private final FolioModuleMetadata folioModuleMetadata;
  private final FolioExecutionContext folioExecutionContext;

  @Override
  public ResponseEntity<SharingInstance> startSharingInstance(UUID consortiumId, @Validated SharingInstance sharingInstance) {
    var centralTenantId = configurationService.getCentralTenantId(folioExecutionContext.getTenantId());
    try (var ignored = new FolioExecutionContextSetter(
      TenantContextUtils.prepareContextForTenant(centralTenantId, folioModuleMetadata, folioExecutionContext))) {
     var sharedInstance = sharingInstanceService.start(consortiumId, sharingInstance);
     return ResponseEntity.status(CREATED).body(sharedInstance);
    }
  }

  @Override
  public ResponseEntity<SharingInstance> getSharingInstanceById(UUID consortiumId, UUID actionId) {
    var centralTenantId = configurationService.getCentralTenantId(folioExecutionContext.getTenantId());
    try (var ignored = new FolioExecutionContextSetter(
      TenantContextUtils.prepareContextForTenant(centralTenantId, folioModuleMetadata, folioExecutionContext))) {
      return ResponseEntity.ok(sharingInstanceService.getById(consortiumId, actionId));
    }
  }

  @Override
  public ResponseEntity<SharingInstanceCollection> getSharingInstances(UUID consortiumId, UUID instanceIdentifier,
      String sourceTenantId, String targetTenantId, Status status, Integer offset, Integer limit) {
    var centralTenantId = configurationService.getCentralTenantId(folioExecutionContext.getTenantId());
    try (var ignored = new FolioExecutionContextSetter(
      TenantContextUtils.prepareContextForTenant(centralTenantId, folioModuleMetadata, folioExecutionContext))) {
      return ResponseEntity.ok(sharingInstanceService.getSharingInstances(consortiumId, instanceIdentifier, sourceTenantId,
        targetTenantId, status, offset, limit));
    }
  }
}
