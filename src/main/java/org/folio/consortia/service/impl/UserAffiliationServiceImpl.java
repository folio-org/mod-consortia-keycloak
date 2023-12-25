package org.folio.consortia.service.impl;

import java.util.Objects;
import java.util.UUID;

import com.fasterxml.jackson.core.JsonProcessingException;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.folio.consortia.config.kafka.KafkaService;
import org.folio.consortia.domain.dto.PrimaryAffiliationEvent;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.dto.UserEvent;
import org.folio.consortia.domain.dto.UserType;
import org.folio.consortia.domain.entity.UserTenantEntity;
import org.folio.consortia.service.PrimaryAffiliationService;
import org.folio.consortia.service.TenantService;
import org.folio.consortia.service.UserAffiliationService;
import org.folio.consortia.service.UserTenantService;
import org.folio.spring.FolioExecutionContext;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.databind.ObjectMapper;

import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@Log4j2
@AllArgsConstructor
public class UserAffiliationServiceImpl implements UserAffiliationService {
  private static final String EVENT_PAYLOAD_COULD_NOT_BE_PARSED = "Skipping user affiliation event because input payload: {} could not be parsed";

  private final UserTenantService userTenantService;
  private final TenantService tenantService;
  private final KafkaService kafkaService;
  private final FolioExecutionContext folioExecutionContext;
  private final PrimaryAffiliationService primaryAffiliationService;
  private final ObjectMapper objectMapper = new ObjectMapper();

  @Override
  @Transactional
  public void createPrimaryUserAffiliation(String eventPayload) {
    String centralTenantId = folioExecutionContext.getTenantId();
    var userEvent = parseUserEvent(eventPayload);
    if (Objects.isNull(userEvent)) {
      log.warn(EVENT_PAYLOAD_COULD_NOT_BE_PARSED, eventPayload);
      return;
    }

    try {
      var tenant = tenantService.getByTenantId(userEvent.getTenantId());

      boolean isPrimaryAffiliationExists = userTenantService
        .checkUserIfHasPrimaryAffiliationByUserId(tenant.getConsortiumId(), userEvent.getUserDto().getId());
      if (isPrimaryAffiliationExists) {
        log.warn("createPrimaryUserAffiliation:: Primary affiliation already exists for tenant/user: {}/{}",
          userEvent.getTenantId(), userEvent.getUserDto().getUsername());
        return;
      }

      PrimaryAffiliationEvent affiliationEvent = createPrimaryAffiliationEvent(userEvent, centralTenantId, tenant.getConsortiumId());
      primaryAffiliationService.createPrimaryAffiliation(tenant.getConsortiumId(), centralTenantId, tenant, affiliationEvent);
    } catch (Exception e) {
      log.error("Exception occurred while creating primary affiliation for userId: {}, tenant: {} and error message: {}",
        userEvent.getUserDto().getId(), userEvent.getTenantId(), e.getMessage(), e);
    }
  }

  @Override
  @Transactional
  public void updatePrimaryUserAffiliation(String eventPayload) {
    String centralTenantId = folioExecutionContext.getTenantId();
    var userEvent = parseUserEvent(eventPayload);
    if (Objects.isNull(userEvent)) {
      log.warn(EVENT_PAYLOAD_COULD_NOT_BE_PARSED, eventPayload);
      return;
    }

    try {
      var tenant = tenantService.getByTenantId(userEvent.getTenantId());
      boolean isPrimaryAffiliationExists = userTenantService
        .checkUserIfHasPrimaryAffiliationByUserId(tenant.getConsortiumId(), userEvent.getUserDto().getId());
      if (!isPrimaryAffiliationExists) {
        log.info("updatePrimaryUserAffiliation:: Started processing case after changing user type from 'patron' to 'staff' for userId: {}, tenant: {}",
          userEvent.getUserDto().getId(), userEvent.getTenantId());
        PrimaryAffiliationEvent affiliationEvent = createPrimaryAffiliationEvent(userEvent, centralTenantId, tenant.getConsortiumId());
        primaryAffiliationService.createPrimaryAffiliation(tenant.getConsortiumId(), centralTenantId, tenant, affiliationEvent);
        return;
      }

      if (UserType.PATRON.getName().equals(userEvent.getUserDto().getType())) {
        log.info("updatePrimaryUserAffiliation:: Started processing case after changing user type from 'staff' to 'patron' for userId: {}, tenant: {}",
          userEvent.getUserDto().getId(), userEvent.getTenantId());
        deletePrimaryAffiliationAndShadowUsers(userEvent, centralTenantId);
        return;
      }

      UUID userId = getUserId(userEvent);
      String newUsername = userEvent.getUserDto().getUsername();

      UserTenantEntity userTenant = userTenantService.getByUserIdAndTenantId(userId, userEvent.getTenantId());
      boolean isUsernameChanged = ObjectUtils.notEqual(userTenant.getUsername(), newUsername);

      if (isUsernameChanged) {
        userTenantService.updateUsernameInPrimaryUserTenantAffiliation(userId, newUsername, userEvent.getTenantId());
        log.info("updatePrimaryUserAffiliation:: Username in primary affiliation has been updated for the user: {}", userEvent.getUserDto().getId());
      }

      if (Boolean.TRUE.equals(userEvent.getIsPersonalDataChanged())) {
        userTenantService.updateShadowUsersFirstAndLastNames(getUserId(userEvent), userEvent.getTenantId());
      }
      PrimaryAffiliationEvent affiliationEvent = createPrimaryAffiliationEvent(userEvent, centralTenantId, null);
      String data = objectMapper.writeValueAsString(affiliationEvent);

      kafkaService.send(KafkaService.Topic.CONSORTIUM_PRIMARY_AFFILIATION_UPDATED, userEvent.getUserDto().getId(), data);
    } catch (Exception e) {
      log.error("Exception occurred while updating primary affiliation for userId: {}, tenant: {} and error message: {}",
        userEvent.getUserDto().getId(), userEvent.getTenantId(), e.getMessage(), e);
    }
  }

  @Override
  @Transactional
  public void deletePrimaryUserAffiliation(String eventPayload) {
    String centralTenantId = folioExecutionContext.getTenantId();
    var userEvent = parseUserEvent(eventPayload);
    if (Objects.isNull(userEvent)) {
      log.warn(EVENT_PAYLOAD_COULD_NOT_BE_PARSED, eventPayload);
      return;
    }

    try {
      deletePrimaryAffiliationAndShadowUsers(userEvent, centralTenantId);
    } catch (Exception e) {
      log.error("Exception occurred while deleting primary affiliation for userId: {}, tenant: {} and error message: {}",
        userEvent.getUserDto().getId(), userEvent.getTenantId(), e.getMessage(), e);
    }
  }

  private UserEvent parseUserEvent(String eventPayload) {
    try {
      var userEvent = objectMapper.readValue(eventPayload, UserEvent.class);
      log.info("parseUserEvent:: Received {} event for userId: {} and tenant: {}",
        userEvent.getAction(), userEvent.getUserDto().getId(), userEvent.getTenantId());
      return userEvent;
    } catch (Exception e) {
      log.error("Could not parse input payload for processing user event", e);
      return null;
    }
  }

  private UUID getUserId(UserEvent userEvent) {
    if (StringUtils.isBlank(userEvent.getUserDto().getId())) {
      throw new IllegalArgumentException("User id is empty");
    }
    return UUID.fromString(userEvent.getUserDto().getId());
  }

  private void deletePrimaryAffiliationAndShadowUsers(UserEvent userEvent, String centralTenantId) throws JsonProcessingException {
    log.info("deletePrimaryAffiliationAndShadowUsers:: Going to delete primary affiliation and all shadow users for userId: {}, tenant: {}",
      userEvent.getUserDto().getId(), userEvent.getTenantId());
    boolean isPrimaryAffiliationExists = userTenantService.deletePrimaryUserTenantAffiliation(getUserId(userEvent));
    if (!isPrimaryAffiliationExists) {
      log.warn("deletePrimaryAffiliationAndShadowUsers:: Primary affiliation does not existed for tenant/user: {}/{}", userEvent.getTenantId(), userEvent.getUserDto().getId());
      return;
    }

    userTenantService.deleteShadowUsers(getUserId(userEvent));

    PrimaryAffiliationEvent affiliationEvent = createPrimaryAffiliationEvent(userEvent, centralTenantId, null);
    String data = objectMapper.writeValueAsString(affiliationEvent);

    kafkaService.send(KafkaService.Topic.CONSORTIUM_PRIMARY_AFFILIATION_DELETED, userEvent.getUserDto().getId(), data);
    log.info("deletePrimaryAffiliationAndShadowUsers:: Primary affiliation has been deleted for the user: {}", userEvent.getUserDto().getId());
  }

  private PrimaryAffiliationEvent createPrimaryAffiliationEvent(UserEvent userEvent, String centralTenantId, UUID consortiumId) {
    PrimaryAffiliationEvent event = new PrimaryAffiliationEvent();
    event.setId(userEvent.getId());
    event.setUserId(UUID.fromString(userEvent.getUserDto().getId()));

    User userDto = userEvent.getUserDto();
    if (StringUtils.isNotBlank(userDto.getUsername())) { // for delete event username will be empty
      event.setUsername(userEvent.getUserDto().getUsername());

      var personalInfo = userDto.getPersonal();
      if (ObjectUtils.isNotEmpty(personalInfo)) {
        event.setEmail(personalInfo.getEmail());
        event.setPhoneNumber(personalInfo.getPhone());
        event.setMobilePhoneNumber(personalInfo.getMobilePhone());
      }
      event.setBarcode(userDto.getBarcode());
      event.setExternalSystemId(userDto.getExternalSystemId());

    }
    event.setTenantId(userEvent.getTenantId());
    event.setCentralTenantId(centralTenantId);
    event.setConsortiumId(consortiumId);
    return event;
  }
}
