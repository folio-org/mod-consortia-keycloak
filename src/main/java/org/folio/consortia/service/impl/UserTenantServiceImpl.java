package org.folio.consortia.service.impl;

import static org.folio.consortia.utils.TenantContextUtils.prepareContextForTenant;

import org.folio.consortia.domain.entity.BaseUserTenantEntity;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.exception.UserAffiliationException;
import org.folio.consortia.repository.InactiveUserTenantRepository;
import org.folio.consortia.repository.UserTenantRepository;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import one.util.streamex.StreamEx;
import org.apache.commons.collections4.CollectionUtils;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.dto.UserTenant;
import org.folio.consortia.domain.dto.UserTenantCollection;
import org.folio.consortia.domain.entity.InactiveUserTenantEntity;
import org.folio.consortia.domain.entity.TenantEntity;
import org.folio.consortia.domain.entity.UserTenantEntity;
import org.folio.consortia.service.ConsortiumService;
import org.folio.consortia.service.CapabilitiesUserService;
import org.folio.consortia.service.TenantService;
import org.folio.consortia.service.UserService;
import org.folio.consortia.service.UserTenantService;
import org.folio.consortia.utils.HelperUtils;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.FolioModuleMetadata;
import org.folio.spring.data.OffsetRequest;
import org.folio.spring.scope.FolioExecutionContextSetter;
import org.folio.spring.service.SystemUserScopedExecutionService;
import org.springframework.core.convert.ConversionService;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Implementation of {@link UserTenantService}.
 * <p>
 * Consortium table will contain only a single record and it will be prohibited to add another record in this table.
 * If another consortium will be created - a new separate DB schema for it will be created that also stores only a
 * single record in the consortium table. So to simplify logic and source code it was decided that we will not store
 * consortiumId in user_tenant table.
 */
@Service
@Log4j2
@RequiredArgsConstructor
public class UserTenantServiceImpl implements UserTenantService {
  private static final String NOT_FOUND_PRIMARY_AFFILIATION_MSG = "User with %s [%s] doesn't have primary affiliation";
  private static final String USER_ID = "userId";
  private static final String TENANT_ID = "tenantId";
  private static final Boolean IS_PRIMARY_TRUE = true;
  private static final Boolean IS_PRIMARY_FALSE = false;
  private final UserTenantRepository userTenantRepository;
  private final InactiveUserTenantRepository inactiveUserTenantRepository;
  private final FolioExecutionContext folioExecutionContext;
  private final ConversionService converter;
  private final ConsortiumService consortiumService;
  private final UserService userService;
  private final FolioModuleMetadata folioModuleMetadata;
  private final CapabilitiesUserService capabilitiesUserService;
  private final TenantService tenantService;
  private final SystemUserScopedExecutionService systemUserScopedExecutionService;

  @Override
  public UserTenantCollection get(UUID consortiumId, Integer offset, Integer limit) {
    consortiumService.checkConsortiumExistsOrThrow(consortiumId);
    var result = new UserTenantCollection();
    Page<UserTenantEntity> userTenantPage = userTenantRepository.getAll(OffsetRequest.of(offset, limit));
    result.setUserTenants(userTenantPage.map(o -> converter.convert(o, UserTenant.class)).getContent());
    result.setTotalRecords((int) userTenantPage.getTotalElements());
    return result;
  }

  @Override
  public UserTenant getById(UUID consortiumId, UUID id) {
    consortiumService.checkConsortiumExistsOrThrow(consortiumId);
    UserTenantEntity userTenantEntity = userTenantRepository.findById(id)
      .orElseThrow(() -> new ResourceNotFoundException("id", String.valueOf(id)));
    return converter.convert(userTenantEntity, UserTenant.class);
  }

  @Override
  public UserTenantCollection getByUsernameAndTenantId(UUID consortiumId, String username, String tenantId) {
    consortiumService.checkConsortiumExistsOrThrow(consortiumId);
    var result = new UserTenantCollection();
    UserTenantEntity userTenantEntity = userTenantRepository.findByUsernameAndTenantId(username, tenantId)
      .orElseThrow(() -> new ResourceNotFoundException("username", username));
    UserTenant userTenant = converter.convert(userTenantEntity, UserTenant.class);

    result.setUserTenants(List.of(userTenant));
    result.setTotalRecords(1);
    return result;
  }

  @Override
  public UserTenantCollection getByUserId(UUID consortiumId, UUID userId, Integer offset, Integer limit) {
    consortiumService.checkConsortiumExistsOrThrow(consortiumId);
    var result = new UserTenantCollection();
    Page<UserTenantEntity> userTenantPage = userTenantRepository.findByUserId(userId, OffsetRequest.of(offset, limit));

    if (userTenantPage.getContent().isEmpty()) {
      throw new ResourceNotFoundException(USER_ID, String.valueOf(userId));
    }

    result.setUserTenants(userTenantPage.stream().map(o -> converter.convert(o, UserTenant.class)).toList());
    result.setTotalRecords((int) userTenantPage.getTotalElements());
    return result;
  }

  @Override
  @Transactional
  public UserTenant save(UUID consortiumId, UserTenant userTenantDto, boolean isSystemUserContextRequired) {
    log.debug("Going to save user with id: {} into tenant: {}", userTenantDto.getUserId(), userTenantDto.getTenantId());
    consortiumService.checkConsortiumExistsOrThrow(consortiumId);

    Optional<UserTenantEntity> userTenant = userTenantRepository.findByUserIdAndIsPrimaryTrue(userTenantDto.getUserId());
    if (userTenant.isEmpty()) {
      log.warn("Could not find user '{}' with primary affiliation in user_tenant table", userTenantDto.getUserId());
      throw new ResourceNotFoundException(String.format(NOT_FOUND_PRIMARY_AFFILIATION_MSG, USER_ID, userTenantDto.getUserId()));
    }

    User shadowUser = userService.prepareShadowUser(userTenantDto.getUserId(), userTenant.get().getTenant().getId());
    if (isSystemUserContextRequired) {
      createOrUpdateShadowUserWithSystemUserContext(userTenantDto.getUserId(), shadowUser, userTenantDto);
    } else {
      createOrUpdateShadowUserWithRequestedContext(userTenantDto.getUserId(), shadowUser, userTenantDto, folioExecutionContext);
    }

    UserTenantEntity userTenantEntity = toEntity(userTenantDto, consortiumId, shadowUser);
    deleteInactiveUserTenant(userTenantEntity.getUserId(), userTenantEntity.getTenant().getId());
    userTenantRepository.save(userTenantEntity);
    log.info("User affiliation added and user created or activated for user id: {} in the tenant: {}", userTenantDto.getUserId(), userTenantDto.getTenantId());

    return converter.convert(userTenantEntity, UserTenant.class);
  }

  @Override
  public void save(UUID consortiumId, User user, TenantEntity tenantEntity) {
    var userTenantEntity = createUserTenantEntity(user, tenantEntity);
    deleteInactiveUserTenant(userTenantEntity.getUserId(), userTenantEntity.getTenant().getId());
    userTenantRepository.save(userTenantEntity);
  }

  @Override
  @Transactional
  public UserTenant createPrimaryUserTenantAffiliation(UUID consortiumId, TenantEntity consortiaTenant, String userId, String username) {
    UserTenantEntity userTenantEntity = new UserTenantEntity();

    userTenantEntity.setId(UUID.randomUUID());
    userTenantEntity.setUserId(UUID.fromString(userId));
    userTenantEntity.setUsername(username);
    userTenantEntity.setTenant(consortiaTenant);
    userTenantEntity.setIsPrimary(IS_PRIMARY_TRUE);

    deleteInactiveUserTenant(userTenantEntity.getUserId(), userTenantEntity.getTenant().getId());
    var createdRecord = userTenantRepository.save(userTenantEntity);
    var userTenant = converter.convert(createdRecord, UserTenant.class);
    log.info("createPrimaryUserTenantAffiliation:: Successfully created primary affiliation for tenant/userId {}/{}", consortiaTenant.getId(), userId);
    return userTenant;
  }

  @Override
  public void updateUsernameInPrimaryUserTenantAffiliation(UUID userId, String username, String tenantId) {
    userTenantRepository.setUsernameByUserIdAndTenantId(username, userId, tenantId);
    log.info("updatePrimaryUserAffiliation:: Username in primary affiliation has been updated for the user: {}", userId);
  }

  @Override
  public UserTenantEntity getByUserIdAndTenantId(UUID userId, String tenantId) {
    return userTenantRepository.findByUserIdAndTenantId(userId, tenantId)
      .orElseThrow(() -> new ResourceNotFoundException(USER_ID + ", " + TENANT_ID, userId + ", " + tenantId));
  }

  @Override
  public boolean existsByUserIdAndTenantId(UUID userId, String tenantId) {
    return userTenantRepository.findByUserIdAndTenantId(userId, tenantId).isPresent();
  }

  @Override
  @Transactional
  public void deleteByUserIdAndTenantId(UUID consortiumId, String tenantId, UUID userId) {
    log.debug("Going to delete user affiliation for user id: {} in the tenant: {}", userId.toString(), tenantId);

    consortiumService.checkConsortiumExistsOrThrow(consortiumId);
    UserTenantEntity userTenantEntity = userTenantRepository.findByUserIdAndTenantId(userId, tenantId)
      .orElseThrow(() -> new ResourceNotFoundException(USER_ID + ", " + TENANT_ID, userId + ", " + tenantId));

    if (Boolean.TRUE.equals(userTenantEntity.getIsPrimary())) {
      log.warn("Primary affiliation could not be deleted from API for user id: {} in the tenant: {}",
        userId.toString(), userTenantEntity.getTenant().getId());
      throw new UserAffiliationException(String.format(UserAffiliationException.USER_HAS_PRIMARY_AFFILIATION_WITH_TENANT, userId, tenantId));
    }

    String centralTenantId = tenantService.getCentralTenantId();
    if (Objects.equals(tenantId, centralTenantId)) {
      log.warn("Affiliation for user id: {} can not be deleted from central tenant id: {}", userId.toString(), centralTenantId);
      throw new UserAffiliationException(String.format(
        UserAffiliationException.AFFILIATION_FROM_CENTRAL_TENANT_CAN_NOT_BE_DELETED, userId, centralTenantId));
    }

    userTenantRepository.deleteByUserIdAndTenantId(userId, tenantId);

    var inactiveUserTenant = InactiveUserTenantEntity.from(userTenantEntity);
    deleteInactiveUserTenant(inactiveUserTenant.getUserId(), inactiveUserTenant.getTenant().getId());
    inactiveUserTenantRepository.save(inactiveUserTenant);
    log.info("Moved user affiliation to inactive table for user id: {} in the tenant: {}", userId.toString(), tenantId);

    try (var ignored = new FolioExecutionContextSetter(prepareContextForTenant(tenantId, folioModuleMetadata, folioExecutionContext))) {
      User user = userService.getById(userId);
      deactivateUser(user);
      log.info("User affiliation deleted and user deactivated for user id: {} in the tenant: {}", userId.toString(), tenantId);
    }
  }

  @Override
  public boolean checkUserIfHasPrimaryAffiliationByUserId(UUID consortiumId, String userId) {
    consortiumService.checkConsortiumExistsOrThrow(consortiumId);
    Optional<UserTenantEntity> optionalUserTenant = userTenantRepository
      .findByUserIdAndIsPrimaryTrue(UUID.fromString(userId));
    return optionalUserTenant.isPresent();
  }

  @Override
  @Transactional
  public boolean deletePrimaryUserTenantAffiliation(UUID userId) {
    return userTenantRepository.deleteByUserIdAndIsPrimaryTrue(userId) == 1;
  }

  @Override
  public void updateShadowUsersNameAndEmail(UUID userId, String originalTenantId) {
    List<UserTenantEntity> userTenantEntities = userTenantRepository.getByUserIdAndIsPrimaryFalse(userId);
    List<InactiveUserTenantEntity> inactiveUserTenantEntities = inactiveUserTenantRepository.getByUserIdAndIsPrimaryFalse(userId);

    if (CollectionUtils.isEmpty(userTenantEntities) && CollectionUtils.isEmpty(inactiveUserTenantEntities)) {
      log.info("No active or inactive shadow users found for user: {}", userId);
      return;
    }

    String username;
    String firstName;
    String lastName;
    String email;
    Set<String> tenantIds;
    try (var ignored = new FolioExecutionContextSetter(prepareContextForTenant(originalTenantId, folioModuleMetadata, folioExecutionContext))) {
      User primaryUser = userService.getById(userId);
      username = primaryUser.getUsername();
      firstName = primaryUser.getPersonal().getFirstName();
      lastName = primaryUser.getPersonal().getLastName();
      email = primaryUser.getPersonal().getEmail();
      tenantIds = getUserEntityTenantIds(userTenantEntities, inactiveUserTenantEntities);
    }

    log.info("Updating shadow users in all tenants (active and inactive) for the user: {}", userId);
    tenantIds.forEach(tenantId -> {
      try (var ignored = new FolioExecutionContextSetter(prepareContextForTenant(tenantId, folioModuleMetadata, folioExecutionContext))) {
        User shadowUser = userService.getById(userId);
        shadowUser.setUsername(HelperUtils.generateShadowUsernameOrDefault(username, shadowUser.getUsername()));
        shadowUser.getPersonal().setFirstName(firstName);
        shadowUser.getPersonal().setLastName(lastName);
        shadowUser.getPersonal().setEmail(email);
        userService.updateUser(shadowUser);
        log.info("Updated shadow user: {} in tenant : {}", userId, tenantId);
      }
    });
  }

  private void createOrUpdateShadowUser(UUID userId, User shadowUser, UserTenant userTenantDto) {
    log.info("createOrUpdateShadowUser:: Going to create or update shadow user with id: {} in the desired tenant: {}", userId.toString(), userTenantDto.getTenantId());
    User user = userService.getById(userId);
    if (Objects.nonNull(user.getActive())) {
      activateUser(user);
    } else {
      createActiveUser(shadowUser);
    }
  }

  private void createOrUpdateShadowUserWithRequestedContext(UUID userId, User shadowUser, UserTenant userTenantDto, FolioExecutionContext folioExecutionContext) {
    try (var ignored = new FolioExecutionContextSetter(prepareContextForTenant(userTenantDto.getTenantId(), folioModuleMetadata, folioExecutionContext))) {
      createOrUpdateShadowUser(userId, shadowUser, userTenantDto);
    }
  }

  private void createOrUpdateShadowUserWithSystemUserContext(UUID userId, User shadowUser, UserTenant userTenantDto) {
    systemUserScopedExecutionService.executeAsyncSystemUserScoped(userTenantDto.getTenantId(), () ->
      createOrUpdateShadowUser(userId, shadowUser, userTenantDto));
  }

  private void createActiveUser(User user) {
    log.info("Creating user with id {}.", user.getId());
    userService.createUser(user);
  }

  private void activateUser(User user) {
    if (Boolean.TRUE.equals(user.getActive())) {
      log.info("User with id '{}' is already active.", user.getId());
    } else {
      user.setActive(true);
      log.info("Updating User with id '{}' with active 'true'. ", user.getId());
      userService.updateUser(user);
    }
  }

  private void deactivateUser(User user) {
    if (Boolean.FALSE.equals(user.getActive())) {
      log.info("User with id '{}' is already not active", user.getId());
    } else {
      user.setActive(false);
      log.info("Updating User with id '{}' with active 'false'. ", user.getId());
      userService.updateUser(user);
    }
  }

  @Override
  public void deleteShadowUsers(UUID userId) {
    List<UserTenantEntity> userTenantEntities = userTenantRepository.getOrphansByUserIdAndIsPrimaryFalse(userId);
    List<InactiveUserTenantEntity> inactiveUserTenantEntities = inactiveUserTenantRepository.getOrphansByUserIdAndIsPrimaryFalse(userId);

    if (CollectionUtils.isEmpty(userTenantEntities) && CollectionUtils.isEmpty(inactiveUserTenantEntities)) {
      log.info("No orphaned shadow users found for user: {}", userId);
      return;
    }

    log.info("Removing orphaned shadow users from all tenants exist in consortia for the user: {}", userId);
    getUserEntityTenantIds(userTenantEntities, inactiveUserTenantEntities).forEach(tenantId -> {
      try (var ignored = new FolioExecutionContextSetter(prepareContextForTenant(tenantId, folioModuleMetadata, folioExecutionContext))) {
        userService.deleteById(userId.toString());
        log.info("Trying to delete permission user for userId={}", userId.toString());
        capabilitiesUserService.deleteUserCapabilitiesAndRoles(userId.toString());
        log.info("Removed shadow user: {} from tenant : {}", userId, tenantId);
      }
    });

    userTenantRepository.deleteOrphansByUserIdAndIsPrimaryFalse(userId);
    inactiveUserTenantRepository.deleteOrphansByUserIdAndIsPrimaryFalse(userId);

  }

  private void deleteInactiveUserTenant(UUID userId, String tenantId) {
    inactiveUserTenantRepository.findByUserIdAndTenantId(userId, tenantId)
      .ifPresent(inactiveUserTenant -> {
        log.info("Found inactive user tenant record for userId: {} and tenantId: {}, deleting it before creating new affiliation", userId, tenantId);
        inactiveUserTenantRepository.delete(inactiveUserTenant);
      });
  }

  private UserTenantEntity toEntity(UserTenant userTenantDto, UUID consortiumId, User user) {
    UserTenantEntity entity = new UserTenantEntity();
    TenantEntity tenant = new TenantEntity();
    tenant.setId(userTenantDto.getTenantId());
    tenant.setName(userTenantDto.getTenantName());
    tenant.setConsortiumId(consortiumId);

    if (Objects.nonNull(userTenantDto.getId())) {
      entity.setId(userTenantDto.getId());
    } else {
      entity.setId(UUID.randomUUID());
    }

    entity.setUserId(userTenantDto.getUserId());
    entity.setUsername(user.getUsername());
    entity.setTenant(tenant);
    entity.setIsPrimary(IS_PRIMARY_FALSE);
    return entity;
  }

  private UserTenantEntity createUserTenantEntity(User user, TenantEntity tenantEntity) {
    return (UserTenantEntity) new UserTenantEntity()
      .setId(UUID.randomUUID())
      .setUserId(UUID.fromString(user.getId()))
      .setIsPrimary(Boolean.FALSE)
      .setUsername(user.getUsername())
      .setTenant(tenantEntity);
  }

  private Set<String> getUserEntityTenantIds(List<UserTenantEntity> userTenantEntities, List<InactiveUserTenantEntity> inactiveUserTenantEntities) {
    return StreamEx.<BaseUserTenantEntity>of(userTenantEntities).append(inactiveUserTenantEntities)
      .map(BaseUserTenantEntity::getTenant)
      .map(TenantEntity::getId)
      .toSet();
  }

}
