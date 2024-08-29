package org.folio.consortia.support;

import static org.folio.spring.integration.XOkapiHeaders.TENANT;
import static org.folio.spring.integration.XOkapiHeaders.TOKEN;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.time.LocalDateTime;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
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
import org.folio.consortia.domain.dto.SharingPolicyDeleteResponse;
import org.folio.consortia.domain.dto.SharingPolicyRequest;
import org.folio.consortia.domain.dto.SharingPolicyResponse;
import org.folio.consortia.domain.dto.SharingRoleCapabilitySetRequest;
import org.folio.consortia.domain.dto.SharingRoleCapabilitySetResponse;
import org.folio.consortia.domain.dto.SharingRoleDeleteResponse;
import org.folio.consortia.domain.dto.SharingRoleRequest;
import org.folio.consortia.domain.dto.SharingRoleResponse;
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
import org.folio.consortia.domain.entity.SharingRoleEntity;
import org.folio.consortia.domain.entity.SharingSettingEntity;
import org.folio.consortia.domain.entity.TenantDetailsEntity;
import org.folio.consortia.domain.entity.TenantEntity;
import org.folio.consortia.domain.entity.UserTenantEntity;
import org.folio.spring.integration.XOkapiHeaders;
import org.springframework.http.HttpMethod;
import org.testcontainers.shaded.org.apache.commons.lang3.RandomStringUtils;

@UtilityClass
public class EntityUtils {
  public static final String CENTRAL_TENANT_ID = "consortium";
  public static final String TENANT_ID = "diku";
  public static final String TENANT_ID_1 = "tenant1";
  public static final String TENANT_ID_2 = "tenant2";

  public static final String SHARING_ROLE_CAPABILITY_SETS_REQUEST_SAMPLE =
    "mockdata/sharing_role_capability_sets/sharing_role_capability_sets_request.json";
  public static final String SHARING_ROLE_CAPABILITY_SETS_WITHOUT_PAYLOAD_REQUEST_SAMPLE =
    "mockdata/sharing_role_capability_sets/sharing_role_capability_sets_request_without_payload.json";

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
    tenantEntity.setIsDeleted(false);
    return tenantEntity;
  }

  public static TenantEntity createTenantEntity() {
    TenantEntity tenantEntity = new TenantEntity();
    tenantEntity.setId("testtenant1");
    tenantEntity.setCode("ABC");
    tenantEntity.setName("testtenant1");
    tenantEntity.setIsCentral(false);
    tenantEntity.setConsortiumId(UUID.randomUUID());
    tenantEntity.setIsDeleted(false);
    return tenantEntity;
  }

  public static TenantEntity createTenantEntity(String id, String name) {
    TenantEntity tenantEntity = new TenantEntity();
    tenantEntity.setId(id);
    tenantEntity.setCode("ABC");
    tenantEntity.setName(name);
    tenantEntity.setIsCentral(false);
    tenantEntity.setIsDeleted(false);
    return tenantEntity;
  }

  public static TenantEntity createTenantEntity(String id) {
    TenantEntity tenantEntity = new TenantEntity();
    tenantEntity.setId(id);
    tenantEntity.setCode("ABC");
    tenantEntity.setName(id);
    tenantEntity.setIsCentral(false);
    tenantEntity.setIsDeleted(false);
    return tenantEntity;
  }

  public static TenantDetailsEntity createTenantDetailsEntity() {
    TenantDetailsEntity tenantDetailsEntity = new TenantDetailsEntity();
    tenantDetailsEntity.setId("testtenant1");
    tenantDetailsEntity.setCode("ABC");
    tenantDetailsEntity.setName("testtenant1");
    tenantDetailsEntity.setIsCentral(false);
    tenantDetailsEntity.setIsDeleted(false);
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
    tenantDetailsEntity.setIsDeleted(false);
    tenantDetailsEntity.setSetupStatus(SetupStatusEnum.IN_PROGRESS);
    return tenantDetailsEntity;
  }

  public static Tenant createTenant(String id) {
    Tenant tenant = new Tenant();
    tenant.setId(id);
    tenant.setName(id);
    tenant.setIsCentral(false);
    tenant.setCode("ABC");
    tenant.setIsDeleted(false);
    return tenant;
  }

  public static Tenant createTenant(String id, String name) {
    Tenant tenant = new Tenant();
    tenant.setId(id);
    tenant.setName(name);
    tenant.setIsCentral(false);
    tenant.setCode("ABC");
    tenant.setIsDeleted(false);
    return tenant;
  }

  public static Tenant createTenant(String id, String name, boolean isCentral) {
    Tenant tenant = new Tenant();
    tenant.setId(id);
    tenant.setName(name);
    tenant.setIsCentral(isCentral);
    tenant.setIsDeleted(false);
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

  public static SharingRoleEntity createSharingRoleEntity(UUID roleId, String tenantId) {
    var entity = new SharingRoleEntity();
    entity.setId(UUID.randomUUID());
    entity.setRoleId(roleId);
    entity.setTenantId(tenantId);
    entity.setIsCapabilitySetsShared(false);
    entity.setIsCapabilitiesShared(false);
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

  public static SharingPolicyResponse createSharingPolicyResponse(UUID createPolicyPcId, UUID updatePolicyPcId) {
    return new SharingPolicyResponse().createPoliciesPCId(createPolicyPcId).updatePoliciesPCId(updatePolicyPcId);
  }


  public static SharingPolicyDeleteResponse createSharingPolicyResponseForDelete(UUID pcId) {
    return new SharingPolicyDeleteResponse().pcId(pcId);
  }

  public static SharingRoleResponse createSharingRoleResponse(UUID createRolePcId, UUID updateRolePcId) {
    return new SharingRoleResponse().createRolesPCId(createRolePcId).updateRolesPCId(updateRolePcId);
  }

  public static SharingRoleCapabilitySetResponse createSharingRoleCapabilitySetResponse(UUID createRoleCapabilitySetsPcId,
                                                                           UUID updateRoleCapabilitySetsPcId) {
    return new SharingRoleCapabilitySetResponse()
      .createRoleCapabilitySetsPCId(createRoleCapabilitySetsPcId)
      .updateRoleCapabilitySetsPCId(updateRoleCapabilitySetsPcId);
  }


  public static SharingRoleDeleteResponse createSharingRoleResponseForDelete(UUID pcId) {
    return new SharingRoleDeleteResponse().pcId(pcId);
  }


  public static TenantCollection createTenantCollection(List<Tenant> tenants) {
    TenantCollection tenantCollection = new TenantCollection();
    tenantCollection.setTenants(tenants);
    tenantCollection.setTotalRecords(tenants.size());
    return tenantCollection;
  }

  public static PublicationRequest createPublicationRequest(SharingSettingRequest sharingPolicyRequest, String method){
    PublicationRequest publicationRequest = new PublicationRequest();
    publicationRequest.setUrl(sharingPolicyRequest.getUrl());
    publicationRequest.setMethod(method);
    final ObjectMapper mapper = new ObjectMapper();
    final ObjectNode root = mapper.createObjectNode();
    root.set("id", mapper.convertValue("1844767a-8367-4926-9999-514c35840399", JsonNode.class));
    root.set("name", mapper.convertValue("ORG-NAME", JsonNode.class));
    root.set("source", mapper.convertValue("consortium", JsonNode.class));
    publicationRequest.setPayload(root);
    return publicationRequest;
  }

  public static PublicationRequest createPublicationRequest(SharingPolicyRequest sharingPolicyRequest, String method){
    PublicationRequest publicationRequest = new PublicationRequest();
    publicationRequest.setUrl(sharingPolicyRequest.getUrl());
    publicationRequest.setMethod(method);
    final ObjectMapper mapper = new ObjectMapper();
    final ObjectNode root = mapper.createObjectNode();
    root.set("id", mapper.convertValue("2844767a-8367-4926-9999-514c35840399", JsonNode.class));
    root.set("name", mapper.convertValue("Policy for role: 004d7a66-c51d-402a-9c9f-3bdcdbbcdbe7", JsonNode.class));
    root.set("source", mapper.convertValue("consortium", JsonNode.class));
    publicationRequest.setPayload(root);
    return publicationRequest;
  }

  public static PublicationRequest createPublicationRequest(SharingRoleRequest sharingRoleRequest, String method){
    PublicationRequest publicationRequest = new PublicationRequest();
    publicationRequest.setUrl(sharingRoleRequest.getUrl());
    publicationRequest.setMethod(method);
    final ObjectMapper mapper = new ObjectMapper();
    final ObjectNode root = mapper.createObjectNode();
    root.set("id", mapper.convertValue("3844767a-8367-4926-9999-514c35840399", JsonNode.class));
    root.set("name", mapper.convertValue("Role for policy: 104d7a66-c51d-402a-9c9f-3bdcdbbcdbe7", JsonNode.class));
    root.set("type", mapper.convertValue("consortium", JsonNode.class));
    publicationRequest.setPayload(root);
    return publicationRequest;
  }

  public static PublicationRequest createPublicationRequest(SharingRoleCapabilitySetRequest request, String method){
    var publicationRequest = new PublicationRequest();
    publicationRequest.setUrl(request.getUrl());
    publicationRequest.setMethod(method);
    final ObjectMapper mapper = new ObjectMapper();
    final ObjectNode root = mapper.createObjectNode();
    var capabilitySetNames = mapper.createArrayNode();
    capabilitySetNames.add("account_item.view");
    capabilitySetNames.add("account_item.create");
    root.set("roleId", mapper.convertValue("4844767a-8367-4926-9999-514c35840399", JsonNode.class));
    root.set("capabilitySetNames", capabilitySetNames);
    root.set("type", mapper.convertValue("consortium", JsonNode.class));
    publicationRequest.setPayload(root);
    return publicationRequest;
  }

  public static PublicationRequest createExceptedPublicationRequest(SharingRoleCapabilitySetRequest request,
                                                                    Set<String> tenantList, HttpMethod method) {
    var expectedPublicationRequest = new PublicationRequest();
    expectedPublicationRequest.setTenants(tenantList);
    expectedPublicationRequest.setMethod(method.toString());
    var url = request.getUrl()
      .replace("capability-sets", request.getRoleId() + "/capability-sets");
    expectedPublicationRequest.setUrl(url);
    final ObjectMapper mapper = new ObjectMapper();
    final ObjectNode root = mapper.createObjectNode();
    root.set("group", mapper.convertValue("space", JsonNode.class));
    root.set("source", mapper.convertValue("user", JsonNode.class));
    expectedPublicationRequest.setPayload(root);
    return expectedPublicationRequest;
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

  public static ObjectNode createJsonNodeForDepartmentPayload() {
    Map<String, String> payload = new HashMap<>();
    payload.put("id", "1844767a-8367-4926-9999-514c35840399");
    payload.put("name", "ORG-NAME");
    payload.put("source", "local");
    ObjectMapper mapper = new ObjectMapper();
    return mapper.convertValue(payload, ObjectNode.class);
  }

  public static ObjectNode createJsonNodeForPolicyPayload() {
    Map<String, String> payload = new HashMap<>();
    payload.put("id", "2844767a-8367-4926-9999-514c35840399");
    payload.put("name", "Policy for role: 004d7a66-c51d-402a-9c9f-3bdcdbbcdbe7");
    payload.put("source", "local");
    ObjectMapper mapper = new ObjectMapper();
    return mapper.convertValue(payload, ObjectNode.class);
  }

  public static ObjectNode createJsonNodeForRolePayload() {
    Map<String, String> payload = new HashMap<>();
    payload.put("id", "3844767a-8367-4926-9999-514c35840399");
    payload.put("name", "Role for policy: 104d7a66-c51d-402a-9c9f-3bdcdbbcdbe7");
    payload.put("type", "local");
    ObjectMapper mapper = new ObjectMapper();
    return mapper.convertValue(payload, ObjectNode.class);
  }

  public static ObjectNode createJsonNodeForRoleCapabilitySetsPayload() {
    Map<String, Object> payload = new HashMap<>();
    payload.put("roleId", "4844767a-8367-4926-9999-514c35840399");
    payload.put("capabilitySetNames", List.of("account_item.view", "account_item.create"));
    payload.put("type", "local");
    ObjectMapper mapper = new ObjectMapper();
    return mapper.convertValue(payload, ObjectNode.class);
  }

  public static ObjectNode createJsonNodeForGroupPayload() {
    Map<String, String> payload = new HashMap<>();
    payload.put("group", "space");
    ObjectMapper mapper = new ObjectMapper();
    return mapper.convertValue(payload, ObjectNode.class);
  }

  public static User createUser(String username) {
    return new User().id(UUID.randomUUID().toString()).username(username);
  }

  public static User createUser(UUID id, String username) {
    return new User().id(id.toString()).username(username);
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
    user.setBarcode("0420690");
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

  public static Map<String, Collection<String>> createOkapiHeaders() {
    Map<String, Collection<String>> map = new HashMap<>();
    map.put(TENANT, List.of("diku"));
    map.put(TOKEN, List.of(TOKEN));
    map.put(XOkapiHeaders.USER_ID, List.of(UUID.randomUUID().toString()));
    return map;
  }
}
