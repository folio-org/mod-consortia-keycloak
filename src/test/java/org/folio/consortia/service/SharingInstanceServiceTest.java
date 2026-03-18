package org.folio.consortia.service;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.folio.consortia.support.TestConstants.ACTION_ID;
import static org.folio.consortia.support.TestConstants.CONSORTIUM_ID;
import static org.folio.consortia.support.EntityUtils.createSharingInstance;
import static org.folio.consortia.support.EntityUtils.createSharingInstanceEntity;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import tools.jackson.databind.json.JsonMapper;
import tools.jackson.databind.node.StringNode;
import org.folio.consortia.config.kafka.KafkaService;
import org.folio.consortia.domain.entity.SharingInstanceEntity;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.ConsortiumRepository;
import org.folio.consortia.repository.SharingInstanceRepository;
import org.folio.consortia.service.impl.SharingInstanceServiceImpl;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import org.folio.consortia.domain.dto.SharingInstance;
import org.folio.consortia.domain.dto.Status;
import org.folio.spring.FolioExecutionContext;
import org.folio.spring.integration.XOkapiHeaders;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.mockito.ArgumentMatchers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.batch.autoconfigure.BatchAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.format.support.FormattingConversionService;
import org.springframework.test.context.bean.override.mockito.MockitoBean;

import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import org.folio.consortia.support.extension.EnablePostgresExtension;

@SpringBootTest
@EnableAutoConfiguration(exclude = BatchAutoConfiguration.class)
@EnablePostgresExtension
class SharingInstanceServiceTest {

  private static final UUID instanceIdentifier = UUID.fromString("5b157ec2-8134-4363-a7b1-c9531a7c6a54");
  private static final String EVENT_PAYLOAD = "{\"instanceIdentifier\":\"5b157ec2-8134-4363-a7b1-c9531a7c6a54\",\"sourceTenantId\":\"college\",\"targetTenantId\":\"mobius\"}";
  private static final Map<String, Collection<String>> headers = new HashMap<>();
  @Autowired
  private SharingInstanceServiceImpl sharingInstanceService;
  @MockitoBean
  private ConsortiumRepository consortiumRepository;
  @MockitoBean
  private ConsortiumService consortiumService;
  @MockitoBean
  private TenantService tenantService;
  @MockitoBean
  private SharingInstanceRepository sharingInstanceRepository;
  @MockitoBean
  private FormattingConversionService conversionService;
  @MockitoBean
  private FolioExecutionContext folioExecutionContext;
  @MockitoBean
  private InventoryService inventoryService;
  @MockitoBean
  private JsonMapper objectMapper;
  @MockitoBean
  private KafkaService kafkaService;

  static {
    headers.put(XOkapiHeaders.TENANT, List.of("mobius"));
  }

  @Test
  void shouldGetSharingInstanceById() {
    SharingInstance expectedSharingInstance = createSharingInstance(ACTION_ID, instanceIdentifier, "college", "mobius");
    SharingInstanceEntity
      savedSharingInstance = createSharingInstanceEntity(ACTION_ID, instanceIdentifier, "college", "mobius");

    when(consortiumRepository.existsById(any())).thenReturn(true);
    when(conversionService.convert(any(), eq(SharingInstance.class))).thenReturn(toDto(savedSharingInstance));
    doNothing().when(tenantService).checkTenantExistsOrThrow(anyString());
    when(sharingInstanceRepository.findById(any())).thenReturn(Optional.of(savedSharingInstance));

    var actualSharingInstance = sharingInstanceService.getById(UUID.randomUUID(), ACTION_ID);

    assertThat(actualSharingInstance.getId()).isEqualTo(expectedSharingInstance.getId());

    verify(sharingInstanceRepository, times(1)).findById(ACTION_ID);
  }

  @Test
  void shouldSaveSharingInstanceWhenSourceTenantNotEqualCentralTenant() throws Exception{
    SharingInstance sharingInstance = createSharingInstance(instanceIdentifier, "college", "mobius");
    SharingInstanceEntity savedSharingInstance = createSharingInstanceEntity(instanceIdentifier, "college", "mobius");
    String event = objectMapper.writeValueAsString(sharingInstance);

    when(consortiumRepository.existsById(any())).thenReturn(true);
    when(conversionService.convert(any(), eq(SharingInstance.class))).thenReturn(toDto(savedSharingInstance));
    doNothing().when(tenantService).checkTenantExistsOrThrow(anyString());
    when(tenantService.getCentralTenantId()).thenReturn("mobius");
    when(sharingInstanceRepository.save(any())).thenReturn(savedSharingInstance);
    when(objectMapper.writeValueAsString(any())).thenReturn(event);

    var expectedSharingInstance = createSharingInstance(instanceIdentifier, "college", "mobius");
    var actualSharingInstance = sharingInstanceService.start(UUID.randomUUID(), sharingInstance);

    assertThat(actualSharingInstance.getInstanceIdentifier()).isEqualTo(expectedSharingInstance.getInstanceIdentifier());
    assertThat(actualSharingInstance.getSourceTenantId()).isEqualTo(expectedSharingInstance.getSourceTenantId());
    assertThat(actualSharingInstance.getTargetTenantId()).isEqualTo(expectedSharingInstance.getTargetTenantId());

    verify(kafkaService, times(1)).send(any(), ArgumentMatchers.eq(sharingInstance.getId().toString()), any());
    verify(sharingInstanceRepository, times(1)).save(any());
  }

  @Test
  void shouldSaveSharingInstanceWhenSourceTenantNotEqualCentralTenantAndKeyIsNull() throws Exception{
    SharingInstance sharingInstance = createSharingInstance(instanceIdentifier, "college", "mobius");
    sharingInstance.setId(null);
    SharingInstanceEntity savedSharingInstance = createSharingInstanceEntity(instanceIdentifier, "college", "mobius");
    String event = objectMapper.writeValueAsString(sharingInstance);

    when(consortiumRepository.existsById(any())).thenReturn(true);
    when(conversionService.convert(any(), eq(SharingInstance.class))).thenReturn(toDto(savedSharingInstance));
    doNothing().when(tenantService).checkTenantExistsOrThrow(anyString());
    when(tenantService.getCentralTenantId()).thenReturn("mobius");
    when(sharingInstanceRepository.save(any())).thenReturn(savedSharingInstance);
    when(objectMapper.writeValueAsString(any())).thenReturn(event);

    var expectedSharingInstance = createSharingInstance(instanceIdentifier, "college", "mobius");
    var actualSharingInstance = sharingInstanceService.start(UUID.randomUUID(), sharingInstance);

    assertThat(actualSharingInstance.getInstanceIdentifier()).isEqualTo(expectedSharingInstance.getInstanceIdentifier());
    assertThat(actualSharingInstance.getSourceTenantId()).isEqualTo(expectedSharingInstance.getSourceTenantId());
    assertThat(actualSharingInstance.getTargetTenantId()).isEqualTo(expectedSharingInstance.getTargetTenantId());

    verify(kafkaService, times(1)).send(any(), anyString(), any());
    verify(sharingInstanceRepository, times(1)).save(any());
  }

  @Test
  void shouldSaveSharingInstanceWhenSourceTenantEqualsCentralTenantAndGetInstanceThrowsException() {
    SharingInstance sharingInstance = createSharingInstance(instanceIdentifier, "mobius", "college");
    SharingInstanceEntity sharingInstanceEntity = new SharingInstanceEntity();

    // skip validation part
    when(consortiumRepository.existsById(any())).thenReturn(true);
    doNothing().when(tenantService).checkTenantExistsOrThrow(anyString());
    when(folioExecutionContext.getOkapiHeaders()).thenReturn(headers);

    when(tenantService.getCentralTenantId()).thenReturn("mobius");
    when(conversionService.convert(any(), eq(SharingInstance.class))).thenReturn(toDto(sharingInstanceEntity));
    when(sharingInstanceRepository.save(any())).thenReturn(sharingInstanceEntity);

    // throw exception when getting inventory instance
    doThrow(RuntimeException.class).when(inventoryService).getById(any());

    // verify status field gets updated and save() method gets called
    sharingInstanceService.start(UUID.randomUUID(), sharingInstance);

    assertThat(sharingInstance.getStatus()).isEqualTo(Status.ERROR);
    verify(sharingInstanceRepository, times(1)).save(any());
  }

  @Test
  void shouldSaveSharingInstanceWhenSourceTenantEqualsCentralTenantAndPostInstanceThrowsException() {
    SharingInstance sharingInstance = createSharingInstance(instanceIdentifier, "mobius", "college");
    SharingInstanceEntity sharingInstanceEntity = new SharingInstanceEntity();

    // skip validation part
    when(consortiumRepository.existsById(any())).thenReturn(true);
    doNothing().when(tenantService).checkTenantExistsOrThrow(anyString());
    when(folioExecutionContext.getOkapiHeaders()).thenReturn(headers);

    when(tenantService.getCentralTenantId()).thenReturn("mobius");
    when(conversionService.convert(any(), eq(SharingInstance.class))).thenReturn(toDto(sharingInstanceEntity));
    when(sharingInstanceRepository.save(any())).thenReturn(sharingInstanceEntity);

    // return instance as JsonNode when getting
    ObjectMapper objectMapper = new ObjectMapper();
    JsonNode inventoryInstance = objectMapper.readTree("{ \"source\" : \"folio\" } ");

    when(inventoryService.getById(any())).thenReturn(inventoryInstance);

    // throw exception when posting inventory instance
    doThrow(RuntimeException.class).when(inventoryService).saveInstance(any());

    // verify status field gets updated and save() method gets called
    sharingInstanceService.start(UUID.randomUUID(), sharingInstance);

    assertThat(sharingInstance.getStatus()).isEqualTo(Status.ERROR);
    verify(sharingInstanceRepository, times(1)).save(any());
  }

  @Test
  void shouldSaveSharingInstanceWhenSourceTenantEqualsCentralTenantAndAllRequestsSuccessful() {
    SharingInstance sharingInstance = createSharingInstance(instanceIdentifier, "mobius", "college");
    SharingInstanceEntity sharingInstanceEntity = new SharingInstanceEntity();

    // skip validation part
    when(consortiumRepository.existsById(any())).thenReturn(true);
    doNothing().when(tenantService).checkTenantExistsOrThrow(anyString());
    when(folioExecutionContext.getOkapiHeaders()).thenReturn(headers);

    when(tenantService.getCentralTenantId()).thenReturn("mobius");
    when(conversionService.convert(any(), eq(SharingInstance.class))).thenReturn(toDto(sharingInstanceEntity));
    when(sharingInstanceRepository.save(any())).thenReturn(sharingInstanceEntity);

    // return instance as JsonNode when getting
    ObjectMapper objectMapper = new ObjectMapper();
    JsonNode inventoryInstance = objectMapper.readTree("{ \"source\" : \"folio\" } ");

    when(inventoryService.getById(any())).thenReturn(inventoryInstance);

    // do nothing when posting inventory instance
    doNothing().when(inventoryService).saveInstance(anyString());

    // verify status field gets updated and save() method gets called
    sharingInstanceService.start(UUID.randomUUID(), sharingInstance);

    assertThat(sharingInstance.getError()).isNull();
    assertThat(sharingInstance.getStatus()).isEqualTo(Status.COMPLETE);
    verify(sharingInstanceRepository, times(1)).save(any());
  }

  @Test
  void shouldUpdatePreviousSharingInstanceWhenAfterSuccessfulUpdate() {
    String sourceTenantId = "mobius";
    String targetTenantId = "college";
    SharingInstance sharingInstance = createSharingInstance(instanceIdentifier, sourceTenantId, targetTenantId);
    SharingInstanceEntity sharingInstanceEntity = new SharingInstanceEntity();
    SharingInstanceEntity existingSharingInstanceEntity = createSharingInstanceEntity(sharingInstance.getId(), instanceIdentifier, sourceTenantId, targetTenantId);
    SharingInstanceEntity updatingSharingInstanceEntity = createSharingInstanceEntity(sharingInstance.getId(), instanceIdentifier, sourceTenantId, targetTenantId);
    updatingSharingInstanceEntity.setStatus(Status.COMPLETE);

    // skip validation part
    when(consortiumRepository.existsById(any())).thenReturn(true);
    doNothing().when(tenantService).checkTenantExistsOrThrow(anyString());
    when(folioExecutionContext.getOkapiHeaders()).thenReturn(headers);

    when(tenantService.getCentralTenantId()).thenReturn("mobius");
    when(conversionService.convert(any(), eq(SharingInstance.class))).thenReturn(toDto(sharingInstanceEntity));
    when(sharingInstanceRepository.findByInstanceAndTenantIds(instanceIdentifier, sourceTenantId, targetTenantId))
      .thenReturn(Optional.of(existingSharingInstanceEntity));

    // existing object should be updated, not created new one
    when(sharingInstanceRepository.save(updatingSharingInstanceEntity)).thenReturn(updatingSharingInstanceEntity);

    // return instance as JsonNode when getting
    ObjectMapper objectMapper = new ObjectMapper();
    JsonNode inventoryInstance = objectMapper.readTree("{ \"source\" : \"folio\" } ");

    when(inventoryService.getById(any())).thenReturn(inventoryInstance);

    // do nothing when posting inventory instance
    doNothing().when(inventoryService).saveInstance(anyString());

    // verify status field gets updated and save() method gets called
    sharingInstanceService.start(UUID.randomUUID(), sharingInstance);

    assertThat(sharingInstance.getError()).isNull();
    assertThat(sharingInstance.getStatus()).isEqualTo(Status.COMPLETE);
    verify(sharingInstanceRepository, times(1)).save(any());
    verify(sharingInstanceRepository, times(1)).findByInstanceAndTenantIds(any(), anyString(), anyString());
  }

  @Test
  void shouldPromoteSharingInstanceWithCompleteStatus() {
    SharingInstance sharingInstance = createSharingInstance(instanceIdentifier, "college", "mobius");
    SharingInstanceEntity sharingInstanceEntity = new SharingInstanceEntity();

    when(tenantService.getCentralTenantId()).thenReturn("mobius");
    doNothing().when(tenantService).checkTenantExistsOrThrow(anyString());
    when(objectMapper.readValue(anyString(), eq(SharingInstance.class))).thenReturn(sharingInstance);
    when(sharingInstanceRepository.findOne(any(Specification.class))).thenReturn(Optional.of(sharingInstanceEntity));

    sharingInstanceService.completePromotingLocalInstance(EVENT_PAYLOAD);

    assertThat(sharingInstanceEntity.getError()).isNull();
    assertThat(sharingInstanceEntity.getStatus()).isEqualTo(Status.COMPLETE);
    verify(sharingInstanceRepository, times(1)).save(any());
  }

  @Test
  void shouldPromoteSharingInstanceWithErrorStatus() {
    SharingInstance sharingInstance = createSharingInstance(instanceIdentifier, "college", "mobius");
    sharingInstance.setError("Promotion failed");
    SharingInstanceEntity sharingInstanceEntity = new SharingInstanceEntity();

    when(tenantService.getCentralTenantId()).thenReturn("mobius");
    doNothing().when(tenantService).checkTenantExistsOrThrow(anyString());
    when(objectMapper.readValue(anyString(), eq(SharingInstance.class))).thenReturn(sharingInstance);
    when(sharingInstanceRepository.findOne(any(Specification.class))).thenReturn(Optional.of(sharingInstanceEntity));

    sharingInstanceService.completePromotingLocalInstance(EVENT_PAYLOAD);

    assertThat(sharingInstanceEntity.getError()).isNotEmpty();
    assertThat(sharingInstanceEntity.getStatus()).isEqualTo(Status.ERROR);
    verify(sharingInstanceRepository, times(1)).save(any());
  }

  @ParameterizedTest
  @CsvSource(value = {
    "folio, CONSORTIUM-FOLIO",
    "marc, CONSORTIUM-MARC",
    "linked_data, CONSORTIUM-LINKED_DATA"
  })
  void shouldChangeInventoryRecordSourceToConsortium(String initial, String expected) {
    var centralTenant = "mobius";
    var targetTenant = "college";
    var sharingInstanceEntity = new SharingInstanceEntity();

    when(consortiumRepository.existsById(any())).thenReturn(true);
    doNothing().when(tenantService).checkTenantExistsOrThrow(anyString());
    when(folioExecutionContext.getOkapiHeaders()).thenReturn(headers);
    when(tenantService.getCentralTenantId()).thenReturn(centralTenant);
    when(conversionService.convert(any(), eq(SharingInstance.class))).thenReturn(toDto(sharingInstanceEntity));
    when(sharingInstanceRepository.save(any())).thenReturn(sharingInstanceEntity);

    var inventoryInstance = new ObjectMapper().createObjectNode().set("source", new StringNode(initial));
    when(inventoryService.getById(any())).thenReturn(inventoryInstance);
    doNothing().when(inventoryService).saveInstance(anyString());

    sharingInstanceService.start(UUID.randomUUID(), createSharingInstance(instanceIdentifier, centralTenant, targetTenant));
    assertThat(inventoryInstance.get("source").asString()).isEqualTo(expected);
  }

  @Test
  void shouldFailWhenSourceIsNotRecognized() {
    var centralTenant = "mobius";
    var targetTenant = "college";
    var sharingInstance = createSharingInstance(instanceIdentifier, centralTenant, targetTenant);
    var sharingInstanceEntity = new SharingInstanceEntity();

    when(consortiumRepository.existsById(any())).thenReturn(true);
    doNothing().when(tenantService).checkTenantExistsOrThrow(anyString());
    when(folioExecutionContext.getOkapiHeaders()).thenReturn(headers);
    when(tenantService.getCentralTenantId()).thenReturn(centralTenant);
    when(conversionService.convert(any(), eq(SharingInstance.class))).thenReturn(toDto(sharingInstanceEntity));
    when(sharingInstanceRepository.save(any())).thenReturn(sharingInstanceEntity);

    var inventoryInstance = new ObjectMapper().createObjectNode().set("source", new StringNode("invalid_source"));
    when(inventoryService.getById(any())).thenReturn(inventoryInstance);
    sharingInstanceService.start(UUID.randomUUID(), sharingInstance);

    assertThat(sharingInstance.getStatus()).isEqualTo(Status.ERROR);
    assertThat(sharingInstance.getError()).contains("Failed to post inventory instance with reason");
    verify(sharingInstanceRepository).findByInstanceAndTenantIds(
      sharingInstance.getInstanceIdentifier(), centralTenant, targetTenant);
    verify(sharingInstanceRepository).save(any());
  }

  /* Negative cases */
  @Test
  void shouldThrowResourceNotFoundExceptionWhenTryingToGetSharingInstanceById() {
    when(consortiumRepository.existsById(any())).thenReturn(true);
    when(sharingInstanceRepository.findById(any())).thenReturn(Optional.empty());

    Assertions.assertThrows(ResourceNotFoundException.class,
      () -> sharingInstanceService.getById(CONSORTIUM_ID, ACTION_ID));
  }

  @Test
  void shouldThrowExceptionWhenTryingToPostSharingInstanceWithMemberTenants() {
    SharingInstance sharingInstance = createSharingInstance(instanceIdentifier, "university", "college");
    when(consortiumRepository.existsById(any())).thenReturn(true);
    doNothing().when(tenantService).checkTenantExistsOrThrow(anyString());
    when(tenantService.getCentralTenantId()).thenReturn("mobius");

    Assertions.assertThrows(IllegalArgumentException.class,
      () -> sharingInstanceService.start(CONSORTIUM_ID, sharingInstance));
  }

  @Test
  void shouldNotPromoteSharingInstanceWhenTargetTenantDoesNotEqualCentralTenant() {
    SharingInstance sharingInstance = createSharingInstance(instanceIdentifier, "college", "mobius");

    when(tenantService.getCentralTenantId()).thenReturn("college");
    doNothing().when(tenantService).checkTenantExistsOrThrow(anyString());
    when(objectMapper.readValue(anyString(), eq(SharingInstance.class))).thenReturn(sharingInstance);

    // call the method and verify there was no call to repository
    sharingInstanceService.completePromotingLocalInstance(EVENT_PAYLOAD);

    verify(sharingInstanceRepository, never()).save(any());
  }

  @Test
  void shouldNotPromoteSharingInstanceWhenSharingInstanceDoesNotExist() {
    SharingInstance sharingInstance = createSharingInstance(instanceIdentifier, "college", "mobius");

    when(tenantService.getCentralTenantId()).thenReturn("mobius");
    doNothing().when(tenantService).checkTenantExistsOrThrow(anyString());
    when(sharingInstanceRepository.findOne(any(Specification.class))).thenReturn(Optional.empty());
    when(objectMapper.readValue(anyString(), eq(SharingInstance.class))).thenReturn(sharingInstance);

    // call the method and verify there was no call to repository
    sharingInstanceService.completePromotingLocalInstance(EVENT_PAYLOAD);

    verify(sharingInstanceRepository, never()).save(any());
  }

  private SharingInstance toDto(SharingInstanceEntity entity) {
    SharingInstance sharingInstance = new SharingInstance();
    sharingInstance.setId(entity.getId());
    sharingInstance.setInstanceIdentifier(entity.getInstanceId());
    sharingInstance.setSourceTenantId(entity.getSourceTenantId());
    sharingInstance.setTargetTenantId(entity.getTargetTenantId());
    sharingInstance.setStatus(entity.getStatus());
    sharingInstance.setError(entity.getError());
    return sharingInstance;
  }
}
