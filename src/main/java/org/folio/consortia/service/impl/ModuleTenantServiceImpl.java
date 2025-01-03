package org.folio.consortia.service.impl;

import static org.folio.consortia.utils.Constants.EUREKA_PLATFORM;

import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;
import org.folio.consortia.client.EurekaProxyTenantsClient;
import org.folio.consortia.client.OkapiClient;
import org.folio.consortia.domain.dto.ModuleForTenant;
import org.folio.consortia.service.ModuleTenantService;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.exception.NotFoundException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import java.net.URI;
import java.util.List;
import java.util.Optional;


@Log4j2
@Service
@RequiredArgsConstructor
public class ModuleTenantServiceImpl implements ModuleTenantService {

  private static final String URL_PREFIX = "http://_";
  private static final String MOD_USERS = "mod-users";
  private static final String MOD_USERS_NOT_FOUND_ERROR = "Module id not found for name: " + MOD_USERS;
  private static final String MOD_USERS_REGEXP = "^mod-users-\\d.*$";

  @Setter
  @Value("${folio.platform}")
  private String platform;

  private final FolioExecutionContext folioExecutionContext;
  private final OkapiClient okapiClient;
  private final EurekaProxyTenantsClient eurekaProxyTenantsClient;

  @Override
  @Cacheable(cacheNames = "modUsersModuleIds")
  public String getModUsersModuleId() {
    var moduleId = StringUtils.equals(EUREKA_PLATFORM, platform) ? getModUsersModuleIdForEureka() : getModUsersModuleIdForOkapi();
    return moduleId.orElseThrow(() -> new NotFoundException(MOD_USERS_NOT_FOUND_ERROR));
  }

  private Optional<String> getModUsersModuleIdForOkapi() {
    var tenantId = folioExecutionContext.getTenantId();
    var modules = okapiClient.getModuleIds(URI.create(URL_PREFIX), tenantId, MOD_USERS);
    if (!modules.isEmpty()) {
      return Optional.of(modules.get(0).getId());
    }
    return Optional.empty();
  }

  private Optional<String> getModUsersModuleIdForEureka() {
    var modules = eurekaProxyTenantsClient.getModules(URI.create(URL_PREFIX), folioExecutionContext.getTenantId());
    return filterModUsersModuleId(modules);
  }

  private Optional<String> filterModUsersModuleId(List<ModuleForTenant> modules) {
    return modules.stream().map(ModuleForTenant::getId).filter(id -> id.matches(MOD_USERS_REGEXP)).findFirst();
  }
}
