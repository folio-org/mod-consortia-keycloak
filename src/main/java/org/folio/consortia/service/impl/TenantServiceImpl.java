package org.folio.consortia.service.impl;

import java.util.List;
import java.util.UUID;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.folio.consortia.domain.dto.Tenant;
import org.folio.consortia.domain.dto.TenantCollection;
import org.folio.consortia.domain.dto.TenantDetails;
import org.folio.consortia.domain.dto.TenantDetails.SetupStatusEnum;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.entity.TenantDetailsEntity;
import org.folio.consortia.domain.entity.TenantEntity;
import org.folio.consortia.domain.entity.UserTenantEntity;
import org.folio.consortia.exception.ResourceAlreadyExistException;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.TenantDetailsRepository;
import org.folio.consortia.repository.TenantRepository;
import org.folio.consortia.repository.UserTenantRepository;
import org.folio.consortia.service.ConsortiumService;
import org.folio.consortia.service.TenantService;
import org.folio.consortia.utils.TenantContextUtils;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.data.OffsetRequest;
import org.folio.spring.scope.FolioExecutionContextSetter;
import org.springframework.core.convert.ConversionService;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Log4j2
@RequiredArgsConstructor
public class TenantServiceImpl implements TenantService {

  private final TenantRepository tenantRepository;
  private final UserTenantRepository userTenantRepository;
  private final TenantDetailsRepository tenantDetailsRepository;
  private final ConversionService converter;
  private final ConsortiumService consortiumService;
  private final FolioExecutionContext folioExecutionContext;

  @Override
  public TenantCollection get(UUID consortiumId, Integer offset, Integer limit) {
    TenantCollection result = new TenantCollection();
    consortiumService.checkConsortiumExistsOrThrow(consortiumId);
    Page<TenantEntity> page = tenantRepository.findByConsortiumId(consortiumId, OffsetRequest.of(offset, limit));
    result.setTenants(page.map(o -> converter.convert(o, Tenant.class)).getContent());
    result.setTotalRecords((int) page.getTotalElements());
    return result;
  }

  @Override
  public TenantCollection getAll(UUID consortiumId) {
    TenantCollection result = new TenantCollection();
    List<Tenant> list = tenantRepository.findByConsortiumId(consortiumId)
      .stream().map(o -> converter.convert(o, Tenant.class)).toList();
    result.setTenants(list);
    result.setTotalRecords(list.size());
    return result;
  }

  @Override
  public TenantDetails getTenantDetailsById(UUID consortiumId, String tenantId) {
    consortiumService.checkConsortiumExistsOrThrow(consortiumId);
    var tenantDetailsEntity = tenantDetailsRepository.findById(tenantId).orElseThrow(() ->
      new ResourceNotFoundException("tenantId", tenantId));
    return converter.convert(tenantDetailsEntity, TenantDetails.class);
  }

  @Override
  public String getCentralTenantId() {
    TenantEntity tenant = tenantRepository.findCentralTenant()
      .orElseThrow(() -> new ResourceNotFoundException("A central tenant is not found. The central tenant must be created"));
    return tenant.getId();
  }

  @Override
  public TenantEntity getByTenantId(String tenantId) {
    return tenantRepository.findById(tenantId)
      .orElse(null);
  }

  @Override
  public boolean centralTenantExists() {
    return tenantRepository.existsByIsCentralTrue();
  }

  @Override
  public Tenant saveTenant(TenantEntity tenantEntity) {
    log.debug("saveTenant:: Trying to save tenant with consoritumId={} and tenant with id={}",
      tenantEntity.getConsortiumId(), tenantEntity.getId());
    TenantEntity savedTenant = tenantRepository.save(tenantEntity);
    log.info("saveTenant: Tenant '{}' successfully saved", savedTenant.getId());
    return converter.convert(savedTenant, Tenant.class);
  }

  @Override
  public Tenant saveTenant(UUID consortiumId, Tenant tenantDto) {
    return saveTenant(toTenantEntity(consortiumId, tenantDto));
  }

  @Override
  public Tenant saveTenantDetails(UUID consortiumId, Tenant tenantDto, TenantDetails.SetupStatusEnum setupStatus) {
    log.debug("saveTenant:: Trying to save tenant with consoritumId={} and tenant with id={}, setupStatus={}",
      consortiumId, tenantDto, setupStatus);
    TenantDetailsEntity entity = toTenantDetailsEntity(consortiumId, tenantDto, setupStatus);
    TenantDetailsEntity savedTenant = tenantDetailsRepository.save(entity);
    log.info("saveTenant: Tenant '{}' successfully saved, setupStatus={}", savedTenant.getId(), savedTenant.getSetupStatus());
    return converter.convert(savedTenant, Tenant.class);
  }

  @Override
  public void saveUserTenant(UUID consortiumId, User user, Tenant tenant) {
    userTenantRepository.save(createUserTenantEntity(consortiumId, user, tenant));
  }

  @Override
  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public void updateTenantSetupStatus(String tenantId, String centralTenantId, SetupStatusEnum setupStatus) {
    try (var ignored = new FolioExecutionContextSetter(TenantContextUtils.prepareContextForTenant(centralTenantId,
      folioExecutionContext.getFolioModuleMetadata(), folioExecutionContext))) {
      tenantDetailsRepository.setSetupStatusByTenantId(setupStatus, tenantId);
      log.info("updateTenantSetupStatus:: tenant id={} status updated to {}", tenantId, setupStatus);
    }
  }

  @Override
  public void checkTenantUniqueNameAndCodeOrThrow(Tenant tenant) {
    if (tenantRepository.existsByNameForOtherTenant(tenant.getName(), tenant.getId())) {
      throw new ResourceAlreadyExistException("name", tenant.getName());
    }
    if (tenantRepository.existsByCodeForOtherTenant(tenant.getCode(), tenant.getId())) {
      throw new ResourceAlreadyExistException("code", tenant.getCode());
    }
  }

  @Override
  public void checkTenantExistsOrThrow(String tenantId) {
    if (!tenantRepository.existsById(tenantId)) {
      throw new ResourceNotFoundException("id", tenantId);
    }
  }

  @Override
  public void checkTenantsAndConsortiumExistsOrThrow(UUID consortiumId, List<String> tenantIds) {
    consortiumService.checkConsortiumExistsOrThrow(consortiumId);
    var tenantEntities = tenantRepository.findAllById(tenantIds);

    if (tenantEntities.size() != tenantIds.size()) {
      var foundTenantIds = tenantEntities.stream()
        .map(TenantEntity::getId)
        .toList();
      String absentTenants = String.join(", ", CollectionUtils.subtract(tenantIds, foundTenantIds));
      log.warn("Tenants with ids {} not found", absentTenants);

      throw new ResourceNotFoundException("ids", absentTenants);
    }
  }

  private TenantEntity toTenantEntity(UUID consortiumId, Tenant tenantDto) {
    TenantEntity entity = new TenantEntity();
    entity.setId(tenantDto.getId());
    entity.setName(tenantDto.getName());
    entity.setCode(tenantDto.getCode());
    entity.setIsCentral(tenantDto.getIsCentral());
    entity.setConsortiumId(consortiumId);
    entity.setIsDeleted(tenantDto.getIsDeleted());
    return entity;
  }

  private TenantDetailsEntity toTenantDetailsEntity(UUID consortiumId, Tenant tenantDto, TenantDetails.SetupStatusEnum setupStatus) {
    TenantDetailsEntity entity = new TenantDetailsEntity();
    entity.setId(tenantDto.getId());
    entity.setName(tenantDto.getName());
    entity.setCode(tenantDto.getCode());
    entity.setIsCentral(tenantDto.getIsCentral());
    entity.setConsortiumId(consortiumId);
    entity.setSetupStatus(setupStatus);
    entity.setIsDeleted(tenantDto.getIsDeleted());
    return entity;
  }

  private UserTenantEntity createUserTenantEntity(UUID consortiumId, User user, Tenant tenant) {
    UserTenantEntity userTenantEntity = new UserTenantEntity();
    TenantEntity tenantEntity = toTenantEntity(consortiumId, tenant);

    userTenantEntity.setUserId(UUID.fromString(user.getId()));
    userTenantEntity.setId(UUID.randomUUID());
    userTenantEntity.setIsPrimary(Boolean.FALSE);
    userTenantEntity.setUsername(user.getUsername());
    userTenantEntity.setTenant(tenantEntity);
    return userTenantEntity;
  }

}
