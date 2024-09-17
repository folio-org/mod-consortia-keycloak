package org.folio.consortia.service.impl;

import static java.lang.String.format;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.consortia.client.CustomFieldsClient;
import org.folio.consortia.config.property.RelatedModulesProperties;
import org.folio.consortia.domain.dto.CustomField;
import org.folio.consortia.domain.dto.CustomFieldType;
import org.folio.consortia.service.CustomFieldService;
import org.springframework.stereotype.Service;

@Service
@Log4j2
@RequiredArgsConstructor
public class CustomFieldServiceImpl implements CustomFieldService {

  public static final String ORIGINAL_TENANT_ID_NAME = "originalTenantId";
  public static final CustomField ORIGINAL_TENANT_ID_CUSTOM_FIELD = CustomField.builder()
    .name(ORIGINAL_TENANT_ID_NAME)
    .entityType("user")
    .helpText("Id of tenant where user created originally")
    .customFieldType(CustomFieldType.TEXTBOX_LONG)
    .visible(false)
    .build();

  private static final String QUERY_PATTERN_NAME = "name==%s";
  private final CustomFieldsClient customFieldsClient;
  private final RelatedModulesProperties relatedModulesProperties;

  @Override
  public void createCustomField(CustomField customField) {
    log.info("createCustomField::creating new custom-field with name {}", customField.getName());
    customFieldsClient.postCustomFields(relatedModulesProperties.getModUsersId(), customField);
    log.info("createCustomField::custom-field with name {} created", customField.getName());
  }

  public CustomField getCustomFieldByName(String name) {
    log.debug("getCustomFieldByName::getting custom-field with name {}.", name);
    return customFieldsClient.getByQuery(relatedModulesProperties.getModUsersId(), format(QUERY_PATTERN_NAME, name))
      .getCustomFields().stream().filter(customField -> customField.getName().equals(name))
      .findFirst()
      .orElse(null);
  }
}
