package org.folio.consortia.service;

import java.util.UUID;

import org.folio.consortia.domain.dto.UserTenant;
import org.folio.consortia.domain.dto.UserTenantCollection;
import org.folio.consortia.domain.entity.TenantEntity;
import org.folio.consortia.domain.entity.UserTenantEntity;

/**
 * Service to work with user tenant associations, it provides ability to add association between user and tenant
 * and search tenants that user assigned to.
 * <p>
 * In methods internal implementation there is checking if consortia exists without joins to the consortium table.
 * Also, user_tenant table does not contain field consortiumId because consortium table will always contain only single
 * consortium. Other consortiums will be stored in separate DB schema.
 */
public interface UserTenantService {

  /**
   * Get user tenant associations collection by query based on consortiumId.
   *
   * @param consortiumId the consortiumId
   * @param offset       the offset
   * @param limit        the limit
   * @return the user tenant associations collection
   */
  UserTenantCollection get(UUID consortiumId, Integer offset, Integer limit);

  /**
   * Get user tenant associations collection by user id based on consortiumId.
   *
   * @param consortiumId the consortiumId
   * @param userId       the user id
   * @return the user tenant associations collection
   */
  UserTenantCollection getByUserId(UUID consortiumId, UUID userId, Integer offset, Integer limit);

  /**
   * Get a user tenant associations collection by username based on consortiumId.
   *
   * @param consortiumId the consortiumId
   * @param username     the username
   * @param tenantId     the tenant id
   * @return the user tenant associations collection
   */
  UserTenantCollection getByUsernameAndTenantId(UUID consortiumId, String username, String tenantId);

  /**
   * Get a user tenant association by userId and TenantId.
   *
   * @param userId     the user id
   * @param tenantId     the tenant id
   * @return the user tenant associations collection
   */
  UserTenantEntity getByUserIdAndTenantId(UUID userId, String tenantId);

  /**
   * Get user tenant association by id based on consortiumId.
   *
   * @param consortiumId the consortiumId
   * @param id           the id
   * @return the user tenant association
   */
  UserTenant getById(UUID consortiumId, UUID id);

  /**
   * Inserts single user_tenant based on consortiumId.
   *
   * @param consortiumId  the consortiumId
   * @param userTenantDto the tenantDto
   * @return userTenantDto
   */
  UserTenant save(UUID consortiumId, UserTenant userTenantDto, boolean isSystemUserContextRequired);

  /**
   * Inserts single user_tenant based on kafka userEventDto.
   *
   * @param consortiumId    the consortiumId
   * @param consortiaTenant the consortiaTenant
   * @param userId  id of existing user
   * @param username    the name of existing user
   * @return userTenantDto
   */
  UserTenant createPrimaryUserTenantAffiliation(UUID consortiumId, TenantEntity consortiaTenant, String userId, String username);

  /**
   * Update username fields of user_tenant based on kafka userEventDto.
   *
   * @param userId  id of existing user
   * @param username    the new name of existing user
   */
  void updateUsernameInPrimaryUserTenantAffiliation(UUID userId, String username, String tenantId);

  /**
   * Deletes user_tenant by userId and tenantId.
   *
   * @param consortiumId id of consortium
   * @param tenantId     id of tenant
   * @param userId       id of user
   */
  void deleteByUserIdAndTenantId(UUID consortiumId, String tenantId, UUID userId);

  /**
   * Update the firstname and the lastname of shadows users in all tenants except tenant where user was originally created.
   *
   * @param userId           id of user
   * @param originalTenantId tenant id where real user was created
   */
  void updateShadowUsersFirstAndLastNames(UUID userId, String originalTenantId);

  /**
   * Check if user has primary affiliation.
   *
   * @param userId id of user in user_tenant table
   * @return true if user has primary affiliation
   */
  boolean checkUserIfHasPrimaryAffiliationByUserId(UUID consortiumId, String userId);

  /**
   * Delete primary user tenant affiliation.
   *
   * @param userId id of user in user_tenant table
   * @return true if some rows were deleted
   */
  boolean deletePrimaryUserTenantAffiliation(UUID userId);

  /**
   * Deletes orphaned shadow_users.
   *
   * @param userId id of user.
   */
  void deleteShadowUsers(UUID userId);
}
