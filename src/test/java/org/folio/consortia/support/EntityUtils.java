package org.folio.consortia.support;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import lombok.experimental.UtilityClass;
import org.folio.consortia.domain.dto.ConsortiaConfiguration;
import org.folio.consortia.domain.dto.Consortium;
import org.folio.consortia.domain.dto.Personal;
import org.folio.consortia.domain.dto.PublicationDetailsResponse;
import org.folio.consortia.domain.dto.PublicationRequest;
import org.folio.consortia.domain.dto.PublicationResult;
import org.folio.consortia.domain.dto.PublicationResultCollection;
import org.folio.consortia.domain.dto.PublicationStatus;
import org.folio.consortia.domain.dto.SharingInstance;
import org.folio.consortia.domain.dto.SharingSettingDeleteResponse;
import org.folio.consortia.domain.dto.SharingSettingRequest;
import org.folio.consortia.domain.dto.SharingSettingResponse;
import org.folio.consortia.domain.dto.Tenant;
import org.folio.consortia.domain.dto.TenantCollection;
import org.folio.consortia.domain.dto.TenantDetails.SetupStatusEnum;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.dto.UserTenant;
import org.folio.consortia.domain.entity.ConsortiaConfigurationEntity;
import org.folio.consortia.domain.entity.ConsortiumEntity;
import org.folio.consortia.domain.entity.PublicationStatusEntity;
import org.folio.consortia.domain.entity.PublicationTenantRequestEntity;
import org.folio.consortia.domain.entity.SharingInstanceEntity;
import org.folio.consortia.domain.entity.SharingSettingEntity;
import org.folio.consortia.domain.entity.TenantDetailsEntity;
import org.folio.consortia.domain.entity.TenantEntity;
import org.folio.consortia.domain.entity.UserTenantEntity;
import org.testcontainers.shaded.org.apache.commons.lang3.RandomStringUtils;

@UtilityClass
public class EntityUtils {

  public static ConsortiumEntity createConsortiumEntity(String id, String name) {
    ConsortiumEntity consortiumEntity = new ConsortiumEntity();
    consortiumEntity.setId(UUID.fromString(id));
    consortiumEntity.setName(name);
    return consortiumEntity;
  }

  public static Consortium createConsortium(String id, String name) {
    Consortium consortium = new Consortium();
    consortium.setId(UUID.fromString(id));
    consortium.setName(name);
    return consortium;
  }

  public static TenantEntity createTenantEntity(String id, String name, String code, Boolean isCentral) {
    TenantEntity tenantEntity = new TenantEntity();
    tenantEntity.setId(id);
    tenantEntity.setCode(code);
    tenantEntity.setName(name);
    tenantEntity.setIsCentral(isCentral);
    tenantEntity.setConsortiumId(UUID.randomUUID());
    return tenantEntity;
  }

  public static TenantEntity createTenantEntity() {
    TenantEntity tenantEntity = new TenantEntity();
    tenantEntity.setId("testtenant1");
    tenantEntity.setCode("ABC");
    tenantEntity.setName("testtenant1");
    tenantEntity.setIsCentral(false);
    tenantEntity.setConsortiumId(UUID.randomUUID());
    return tenantEntity;
  }

  public static TenantEntity createTenantEntity(String id, String name) {
    TenantEntity tenantEntity = new TenantEntity();
    tenantEntity.setId(id);
    tenantEntity.setCode("ABC");
    tenantEntity.setName(name);
    tenantEntity.setIsCentral(false);
    return tenantEntity;
  }

  public static TenantDetailsEntity createTenantDetailsEntity() {
    TenantDetailsEntity tenantDetailsEntity = new TenantDetailsEntity();
    tenantDetailsEntity.setId("testtenant1");
    tenantDetailsEntity.setCode("ABC");
    tenantDetailsEntity.setName("testtenant1");
    tenantDetailsEntity.setIsCentral(false);
    tenantDetailsEntity.setConsortiumId(UUID.randomUUID());
    tenantDetailsEntity.setSetupStatus(SetupStatusEnum.COMPLETED);
    return tenantDetailsEntity;
  }

  public static TenantDetailsEntity createTenantDetailsEntity(String id, String name) {
    TenantDetailsEntity tenantDetailsEntity = new TenantDetailsEntity();
    tenantDetailsEntity.setId(id);
    tenantDetailsEntity.setCode("ABC");
    tenantDetailsEntity.setName(name);
    tenantDetailsEntity.setIsCentral(false);
    tenantDetailsEntity.setSetupStatus(SetupStatusEnum.IN_PROGRESS);
    return tenantDetailsEntity;
  }

  public static Tenant createTenant(String id, String name) {
    Tenant tenant = new Tenant();
    tenant.setId(id);
    tenant.setName(name);
    tenant.setIsCentral(false);
    tenant.setCode("ABC");
    return tenant;
  }

  public static Tenant createTenant(String id, String name, boolean isCentral) {
    Tenant tenant = new Tenant();
    tenant.setId(id);
    tenant.setName(name);
    tenant.setIsCentral(isCentral);
    tenant.setCode("ABC");
    return tenant;
  }

  public static UserTenant createUserTenant(UUID associationId) {
    UserTenant userTenant = new UserTenant();
    userTenant.setId(associationId);
    userTenant.setUserId(UUID.randomUUID());
    userTenant.setUsername("username");
    userTenant.setTenantId(String.valueOf(UUID.randomUUID()));
    userTenant.setIsPrimary(true);
    return userTenant;
  }

  public static UserTenantEntity createUserTenantEntity(UUID associationId) {
    UserTenantEntity userTenantEntity = new UserTenantEntity();
    userTenantEntity.setId(associationId);
    userTenantEntity.setTenant(new TenantEntity());
    userTenantEntity.setUsername("username");
    userTenantEntity.setUserId(UUID.randomUUID());
    userTenantEntity.setIsPrimary(false);
    return userTenantEntity;
  }

  public static ConsortiaConfigurationEntity createConsortiaConfigurationEntity(String centralTenantId) {
    ConsortiaConfigurationEntity configuration = new ConsortiaConfigurationEntity();
    configuration.setId(UUID.randomUUID());
    configuration.setCentralTenantId(centralTenantId);
    return configuration;
  }

  public static ConsortiaConfiguration createConsortiaConfiguration(String centralTenantId) {
    ConsortiaConfiguration configuration = new ConsortiaConfiguration();
    configuration.setId(UUID.randomUUID());
    configuration.setCentralTenantId(centralTenantId);
    return configuration;
  }

  public static SharingInstance createSharingInstance(UUID instanceIdentifier, String sourceTenantId, String targetTenantId) {
    SharingInstance sharingInstance = new SharingInstance();
    sharingInstance.setId(TestConstants.ACTION_ID);
    sharingInstance.setInstanceIdentifier(instanceIdentifier);
    sharingInstance.setSourceTenantId(sourceTenantId);
    sharingInstance.setTargetTenantId(targetTenantId);
    return sharingInstance;
  }

  public static SharingInstance createSharingInstance(UUID actionId, UUID instanceIdentifier, String sourceTenantId,
    String targetTenantId) {
    SharingInstance sharingInstance = new SharingInstance();
    sharingInstance.setId(actionId);
    sharingInstance.setInstanceIdentifier(instanceIdentifier);
    sharingInstance.setSourceTenantId(sourceTenantId);
    sharingInstance.setTargetTenantId(targetTenantId);
    return sharingInstance;
  }

  public static SharingInstanceEntity createSharingInstanceEntity(UUID instanceIdentifier, String sourceTenantId, String targetTenantId) {
    SharingInstanceEntity sharingInstance = new SharingInstanceEntity();
    sharingInstance.setId(TestConstants.ACTION_ID);
    sharingInstance.setInstanceId(instanceIdentifier);
    sharingInstance.setSourceTenantId(sourceTenantId);
    sharingInstance.setTargetTenantId(targetTenantId);
    sharingInstance.setCreatedDate(LocalDateTime.now());
    sharingInstance.setCreatedBy(UUID.fromString("dcfc317b-0d7c-4334-8656-596105fa6c99"));
    return sharingInstance;
  }

  public static SharingInstanceEntity createSharingInstanceEntity(UUID actionId, UUID instanceIdentifier, String sourceTenantId, String targetTenantId) {
    SharingInstanceEntity sharingInstance = new SharingInstanceEntity();
    sharingInstance.setId(actionId);
    sharingInstance.setInstanceId(instanceIdentifier);
    sharingInstance.setSourceTenantId(sourceTenantId);
    sharingInstance.setTargetTenantId(targetTenantId);
    sharingInstance.setCreatedDate(LocalDateTime.now());
    sharingInstance.setCreatedBy(UUID.fromString("dcfc317b-0d7c-4334-8656-596105fa6c99"));
    return sharingInstance;
  }

  public static PublicationStatusEntity createPublicationStatusEntity(PublicationStatus publicationStatus) {
    var entity = new PublicationStatusEntity();
    entity.setId(UUID.randomUUID());
    entity.setStatus(publicationStatus);
    entity.setTotalRecords(0);
    return entity;
  }

  public static SharingSettingEntity createSharingSettingEntity(UUID settingId, String tenantId) {
    var entity = new SharingSettingEntity();
    entity.setId(UUID.randomUUID());
    entity.setSettingId(settingId);
    entity.setTenantId(tenantId);
    return entity;
  }

  public static PublicationTenantRequestEntity createPublicationTenantRequestEntity(
    PublicationStatusEntity publicationStatusEntity,
      String tenant, PublicationStatus status, int statusCode) {
    PublicationTenantRequestEntity entity = new PublicationTenantRequestEntity();
    entity.setId(UUID.randomUUID());
    entity.setTenantId(tenant);
    entity.setStatus(status);
    entity.setPcState(publicationStatusEntity);
    entity.setResponse(RandomStringUtils.random(10));
    entity.setRequestPayload(RandomStringUtils.random(10));
    entity.setResponseStatusCode(statusCode);
    entity.setCreatedDate(LocalDateTime.now());
    return entity;
  }

  public static SharingSettingResponse createSharingSettingResponse(UUID createSettingsPcId, UUID updateSettingsPcId) {
    return new SharingSettingResponse().createSettingsPCId(createSettingsPcId).updateSettingsPCId(updateSettingsPcId);
  }

  public static SharingSettingDeleteResponse createSharingSettingResponseForDelete(UUID pcId) {
    return new SharingSettingDeleteResponse().pcId(pcId);
  }

  public static TenantCollection createTenantCollection(List<Tenant> tenants) {
    TenantCollection tenantCollection = new TenantCollection();
    tenantCollection.setTenants(tenants);
    tenantCollection.setTotalRecords(tenants.size());
    return tenantCollection;
  }

  public static PublicationRequest createPublicationRequestForSetting(SharingSettingRequest sharingSetting, String method){
    PublicationRequest publicationRequest = new PublicationRequest();
    publicationRequest.setUrl(sharingSetting.getUrl());
    publicationRequest.setMethod(method);
    final ObjectMapper mapper = new ObjectMapper();
    final ObjectNode root = mapper.createObjectNode();
    root.set("id", mapper.convertValue("1844767a-8367-4926-9999-514c35840399", JsonNode.class));
    root.set("name", mapper.convertValue("ORG-NAME", JsonNode.class));
    root.set("source", mapper.convertValue("consortium", JsonNode.class));
    publicationRequest.setPayload(root);
    return publicationRequest;
  }

  public static PublicationResultCollection createPublicationResultCollection(String tenantId1, String tenantId2) {
    var pbr1 = new PublicationResult();
    pbr1.setTenantId(tenantId1);
    pbr1.setStatusCode(400);
    var pbr2 = new PublicationResult();
    pbr2.setTenantId(tenantId2);
    pbr2.setStatusCode(401);
    var publicationResultCollection = new PublicationResultCollection();
    publicationResultCollection.setPublicationResults(List.of(pbr1, pbr2));
    return publicationResultCollection;
  }

  public static PublicationDetailsResponse createPublicationDetails(PublicationStatus status) {
    PublicationDetailsResponse pbd = new PublicationDetailsResponse();
    pbd.setStatus(status);
    return pbd;
  }

  public static JsonNode createJsonNodeForDepartmentPayload() throws JsonProcessingException {
    Map<String, String> payload = new HashMap<>();
    payload.put("id", "1844767a-8367-4926-9999-514c35840399");
    payload.put("name", "ORG-NAME");
    payload.put("source", "local");
    ObjectMapper mapper = new ObjectMapper();
    String json = mapper.writeValueAsString(payload);
    return mapper.readTree(json);
  }

  public static JsonNode createJsonNodeForGroupPayload() throws JsonProcessingException {
    Map<String, String> payload = new HashMap<>();
    payload.put("group", "space");
    ObjectMapper mapper = new ObjectMapper();
    String json = mapper.writeValueAsString(payload);
    return mapper.readTree(json);
  }

  public static User createUser(String username) {
    return new User().id(UUID.randomUUID().toString()).username(username);
  }

  public static User createUserEntity(Boolean updateble) {
    User user = new User();
    Personal personal = new Personal();
    personal.setPreferredContactTypeId("email");
    personal.setEmail("Test@mail.com");
    personal.setFirstName("testFirst");
    personal.setLastName("testLast");
    user.setId(UUID.randomUUID().toString());
    user.setPatronGroup(null);
    user.setUsername("xyz");
    user.setPersonal(personal);
    user.setActive(Boolean.FALSE.equals(updateble));
    return user;
  }

  public static User createUserEntity(UUID userId) {
    User user = new User();
    Personal personal = new Personal();
    personal.setPreferredContactTypeId("email");
    personal.setEmail("Test@mail.com");
    personal.setFirstName("firstName");
    personal.setLastName("lastName");
    user.setId(userId.toString());
    user.setPatronGroup(null);
    user.setUsername("xyz");
    user.setPersonal(personal);
    user.setActive(true);
    return user;
  }
}
