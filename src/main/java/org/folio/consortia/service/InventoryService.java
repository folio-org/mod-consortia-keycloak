package org.folio.consortia.service;

import java.util.UUID;

import tools.jackson.databind.JsonNode;

public interface InventoryService {

  /**
   * Get an instance by id
   * @param instanceId UUID of the instance
   * @return Instance as JsonNode
   */
  JsonNode getById(UUID instanceId);

  /**
   * Create instance.
   * @param instance instance.
   */
  void saveInstance(Object instance);
}
