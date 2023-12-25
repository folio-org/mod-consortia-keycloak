package org.folio.consortia.service;

import static org.folio.consortia.utils.EntityUtils.CENTRAL_TENANT_ID;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;

import org.folio.consortia.client.CustomFieldsClient;
import org.folio.consortia.config.property.RelatedModulesProperties;
import org.folio.consortia.domain.dto.CustomField;
import org.folio.consortia.domain.dto.CustomFieldCollection;
import org.folio.consortia.domain.dto.CustomFieldType;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.service.impl.CustomFieldServiceImpl;
import java.net.URI;
import java.util.List;

import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import org.folio.spring.FolioExecutionContext;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.batch.BatchAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
@EnableAutoConfiguration(exclude = BatchAutoConfiguration.class)
class CustomFieldServiceTest {
  @InjectMocks
  CustomFieldServiceImpl customFieldService;
  @Mock
  CustomFieldsClient customFieldsClient;
  @Mock
  RelatedModulesProperties relatedModulesProperties;
  @Mock
  FolioExecutionContext folioExecutionContext;

  private static final CustomField ORIGINAL_TENANT_ID_CUSTOM_FIELD = CustomField.builder()
    .name("originalTenantId")
    .entityType("user")
    .helpText("id of tenant where user created originally")
    .customFieldType(CustomFieldType.TEXTBOX_LONG)
    .visible(false)
    .build();

  @Test
  void shouldCreateCustomField() {
    CustomField customField = CustomField.builder().build();
    when(folioExecutionContext.getTenantId()).thenReturn(CENTRAL_TENANT_ID);
    when(relatedModulesProperties.getModUsersId()).thenReturn("USERS");
    Mockito.doNothing().when(customFieldsClient).postCustomFields(any(), eq(customField));
    customFieldService.createCustomField(customField);

    Mockito.verify(customFieldsClient).postCustomFields(any(), any());
    Mockito.verify(relatedModulesProperties, times(1)).getModUsersId();
  }

  @Test
  void shouldGetCustomField() {
    CustomFieldCollection customFieldCollection = new CustomFieldCollection();
    customFieldCollection.setCustomFields(List.of(ORIGINAL_TENANT_ID_CUSTOM_FIELD));
    customFieldCollection.setTotalRecords(1);
    when(folioExecutionContext.getTenantId()).thenReturn(CENTRAL_TENANT_ID);
    when(relatedModulesProperties.getModUsersId()).thenReturn("USERS");
    when(customFieldsClient.getByQuery(any(), eq("name==originalTenantId"))).thenReturn(customFieldCollection);
    var customFields = customFieldService.getCustomFieldByName("originalTenantId");

    Assertions.assertEquals("originalTenantId", customFields.getName());
    Mockito.verify(customFieldsClient, times(1)).getByQuery(any(), any());
    Mockito.verify(relatedModulesProperties, times(1)).getModUsersId();
  }
}
