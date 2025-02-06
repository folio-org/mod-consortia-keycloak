package org.folio.consortia.controller;

import static org.springframework.http.HttpStatus.CREATED;
import static org.springframework.http.HttpStatus.NO_CONTENT;

import java.util.UUID;

import org.folio.consortia.domain.dto.SyncPrimaryAffiliationBody;
import org.folio.consortia.domain.dto.Tenant;
import org.folio.consortia.domain.dto.TenantCollection;
import org.folio.consortia.domain.dto.TenantDeleteRequest;
import org.folio.consortia.domain.dto.TenantDetails;
import org.folio.consortia.domain.dto.TenantDetails.SetupStatusEnum;
import org.folio.consortia.rest.resource.TenantsApi;
import org.folio.consortia.service.SyncPrimaryAffiliationService;
import org.folio.consortia.service.TenantManager;
import org.folio.consortia.service.TenantService;
import org.jetbrains.annotations.NotNull;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@RestController
@RequestMapping("/consortia/{consortiumId}")
@Log4j2
@RequiredArgsConstructor
public class TenantController implements TenantsApi {

  private final TenantService tenantService;
  private final TenantManager tenantManager;
  private final SyncPrimaryAffiliationService syncPrimaryAffiliationService;

  @Override
  public ResponseEntity<TenantCollection> getTenants(UUID consortiumId, Integer offset, Integer limit) {
    return ResponseEntity.ok(tenantManager.get(consortiumId, offset, limit));
  }

  @Override
  public ResponseEntity<Tenant> saveTenant(UUID consortiumId, @Validated Tenant tenant, UUID adminUserId) {
    return ResponseEntity.status(CREATED).body(tenantManager.save(consortiumId, adminUserId, tenant));
  }

  @Override
  public ResponseEntity<Tenant> updateTenant(UUID consortiumId, String tenantId, @Validated Tenant tenant) {
    return ResponseEntity.ok(tenantManager.update(consortiumId, tenantId, tenant));
  }

  @Override
  public ResponseEntity<Void> deleteTenantById(UUID consortiumId, String tenantId, TenantDeleteRequest tenantDeleteRequest) {
    tenantManager.delete(consortiumId, tenantId, tenantDeleteRequest);
    return ResponseEntity.status(NO_CONTENT).build();
  }

  @Override
  public ResponseEntity<TenantDetails> getTenantDetailsById(UUID consortiumId, String tenantId) {
    return ResponseEntity.ok(tenantService.getTenantDetailsById(consortiumId, tenantId));
  }

  @Override
  public ResponseEntity<Void> syncPrimaryAffiliations(UUID consortiumId, String tenantId, @NotNull String centralTenantId) {
    try {
      syncPrimaryAffiliationService.syncPrimaryAffiliations(consortiumId, tenantId, centralTenantId);
      return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    } catch (Exception e) {
      log.error("syncPrimaryAffiliations:: error syncing user primary affiliations", e);
      tenantService.updateTenantSetupStatus(tenantId, centralTenantId, SetupStatusEnum.FAILED);
      throw e;
    }
  }

  @Override
  public ResponseEntity<Void> createPrimaryAffiliations(UUID consortiumId, String tenantId, @NotNull String centralTenantId,
      SyncPrimaryAffiliationBody syncPrimaryAffiliationBody) {
    try {
      syncPrimaryAffiliationService.createPrimaryUserAffiliations(consortiumId, centralTenantId, syncPrimaryAffiliationBody);
      return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
    } catch (Exception e) {
      log.error("createPrimaryAffiliations:: error creating user primary affiliations", e);
      tenantService.updateTenantSetupStatus(tenantId, centralTenantId, SetupStatusEnum.FAILED);
      throw e;
    }
  }
}
