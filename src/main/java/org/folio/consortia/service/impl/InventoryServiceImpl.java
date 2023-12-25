package org.folio.consortia.service.impl;

import java.util.UUID;

import org.folio.consortia.client.InventoryClient;
import org.folio.consortia.service.InventoryService;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.JsonNode;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@Log4j2
@RequiredArgsConstructor
public class InventoryServiceImpl implements InventoryService {
  private final InventoryClient inventoryClient;
  @Override
  public JsonNode getById(UUID instanceId) {
    log.debug("getById:: parameters instanceId: {}", instanceId);
    return inventoryClient.getInstanceById(instanceId.toString());
  }

  @Override
  public void saveInstance(Object instance) {
    log.debug("saveInstance:: Trying to save an instance");
    inventoryClient.saveInstance(instance);
  }
}
