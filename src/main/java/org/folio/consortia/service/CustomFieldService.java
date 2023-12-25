package org.folio.consortia.service;

import org.folio.consortia.domain.dto.CustomField;

/**
 * Each shadow user created by consortia should have a field OriginalTenantId with value of its tenantId
 * in which it was originally created.
 * At the time of enabling tenant createCustomField() will be called.
 */
public interface CustomFieldService {

  /**
   * Creates custom-field.
   *
   * @param customField customField.
   *
   */
  void createCustomField(CustomField customField);

  /**
   * Gets custom-field.
   *
   * @param name name of custom-field.
   *
   */
  CustomField getCustomFieldByName(String name);
}
