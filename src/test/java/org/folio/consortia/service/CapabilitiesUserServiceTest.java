package org.folio.consortia.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.folio.consortia.base.BaseIT.asJsonString;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import feign.FeignException;
import java.util.List;
import java.util.UUID;
import org.folio.common.domain.model.error.Error;
import org.folio.common.domain.model.error.ErrorResponse;
import org.folio.consortia.client.CapabilitySetsClient;
import org.folio.consortia.client.UserCapabilitiesClient;
import org.folio.consortia.client.UserCapabilitySetsClient;
import org.folio.consortia.client.UserPermissionsClient;
import org.folio.consortia.domain.dto.CapabilitySet;
import org.folio.consortia.domain.dto.CapabilitySets;
import org.folio.consortia.domain.dto.PermissionUser;
import org.folio.consortia.domain.dto.User;
import org.folio.consortia.domain.dto.UserCapabilitySetsRequest;
import org.folio.consortia.service.impl.CapabilitiesUserService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.batch.BatchAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
@EnableAutoConfiguration(exclude = BatchAutoConfiguration.class)
class CapabilitiesUserServiceTest {
  private static final String PERMISSIONS_FILE_PATH = "permissions/test-user-permissions.csv";
  private static final String EMPTY_PERMISSIONS_FILE_PATH = "permissions/test-user--empty-permissions.csv";
  @InjectMocks
  CapabilitiesUserService capabilitiesUserService;
  @Mock
  CapabilitySetsClient capabilitySetsClient;
  @Mock
  UserCapabilitiesClient userCapabilitiesClient;
  @Mock
  UserCapabilitySetsClient userCapabilitySetsClient;
  @Mock
  UserPermissionsClient userPermissionsClient;
  @Mock
  ObjectMapper objectMapper;
  @Mock
  FeignException feignException;

  @Test
  void shouldThrowErrorForEmptyPermissionFileWhileCreating() {
    Assertions.assertThrows(java.lang.IllegalStateException.class,
      () -> capabilitiesUserService.createWithPermissionSetsFromFile(UUID.randomUUID().toString(),
        EMPTY_PERMISSIONS_FILE_PATH));
  }

  @Test
  void createWithEmptyPermissions_notImplemented() {
    Assertions.assertThrows(java.lang.UnsupportedOperationException.class,
      () -> capabilitiesUserService.createWithEmptyPermissions(UUID.randomUUID().toString()));
  }

  @Test
  void shouldDeletePermissionUser() {
    String permissionUserId = UUID.randomUUID().toString();
    capabilitiesUserService.deletePermissionUser(permissionUserId);
    verify(userCapabilitiesClient).deleteUserCapabilities(permissionUserId);
  }

  @Test
  void getByUserId_positive() {
    String userId = UUID.randomUUID().toString();
    var permissionUser = PermissionUser.of(null, userId, List.of("test.item.get"));
    when(userPermissionsClient.getPermissionsForUser(userId, false)).thenReturn(permissionUser);

    var actual = capabilitiesUserService.getByUserId(userId);

    assertThat(actual.isPresent()).isTrue();
    assertThat(actual.get()).isEqualTo(permissionUser);
  }

  @Test
  void createWithPermissionsFromFile_positive_alreadyAssigned() throws JsonProcessingException {
    var errorResponse = nothingToUpdateError();
    var user = user();
    var capabilitySetId = UUID.randomUUID();
    var capabilitySetIds = List.of(capabilitySetId);
    var capabilities = new CapabilitySets().addCapabilitySetsItem(new CapabilitySet().id(capabilitySetId));
    var userId = user.getId();
    var expectedRequest = userCapabilitySetsRequest(userId, capabilitySetIds);

    when(capabilitySetsClient.queryCapabilitySets(any(), anyInt(), anyInt())).thenReturn(capabilities);
    when(objectMapper.readValue(anyString(), eq(ErrorResponse.class))).thenReturn(errorResponse);
    when(feignException.contentUTF8()).thenReturn(asJsonString(errorResponse));
    doThrow(feignException).when(userCapabilitySetsClient).assignUserCapabilitySets(userId, expectedRequest);

    capabilitiesUserService.createWithPermissionSetsFromFile(userId, PERMISSIONS_FILE_PATH);

    verify(objectMapper).readValue(anyString(), eq(ErrorResponse.class));
    verify(userCapabilitySetsClient).assignUserCapabilitySets(userId, expectedRequest);
  }

  @Test
  void createWithPermissionsFromFile_negative_unknownError() throws JsonProcessingException {
    var errorResponse = new ErrorResponse()
      .addErrorsItem(new Error().message("failure1"))
      .totalRecords(1);
    var user = user();
    var capabilitySetId = UUID.randomUUID();
    var capabilitySetIds = List.of(capabilitySetId);
    var capabilitySets = new CapabilitySets().addCapabilitySetsItem(new CapabilitySet().id(capabilitySetId));
    var userId = user.getId();
    var expectedRequest = userCapabilitySetsRequest(userId, capabilitySetIds);

    when(capabilitySetsClient.queryCapabilitySets(any(), anyInt(), anyInt())).thenReturn(capabilitySets);
    when(objectMapper.readValue(anyString(), eq(ErrorResponse.class))).thenReturn(errorResponse);
    when(feignException.contentUTF8()).thenReturn(asJsonString(new ErrorResponse()));
    doThrow(feignException).when(userCapabilitySetsClient).assignUserCapabilitySets(userId, expectedRequest);

    assertThatThrownBy(() -> capabilitiesUserService.createWithPermissionSetsFromFile(userId, PERMISSIONS_FILE_PATH))
      .isInstanceOf(FeignException.class);

    verify(objectMapper).readValue(anyString(), eq(ErrorResponse.class));
    verify(userCapabilitySetsClient).assignUserCapabilitySets(userId, expectedRequest);
  }

  private static ErrorResponse nothingToUpdateError() {
    var err = new Error().message("Nothing to update, user-capability relations are not changed");
    return new ErrorResponse().addErrorsItem(err).totalRecords(1);
  }

  private static User user() {
    return new User()
      .id(UUID.randomUUID().toString())
      .username("test-username");
  }

  private static UserCapabilitySetsRequest userCapabilitySetsRequest(String userId, List<UUID> capabilitySetIds) {
    return new UserCapabilitySetsRequest().userId(userId).capabilitySetIds(capabilitySetIds);
  }
}
