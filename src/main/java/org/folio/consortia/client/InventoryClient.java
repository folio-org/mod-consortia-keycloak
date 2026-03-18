package org.folio.consortia.client;

import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;
import org.springframework.web.service.annotation.PostExchange;

import tools.jackson.databind.JsonNode;

import org.springframework.web.client.HttpStatusCodeException;

@HttpExchange("inventory")
public interface InventoryClient {

  @GetExchange(value = "instances/{instanceId}")
  JsonNode getInstanceById(@PathVariable String instanceId);

  @PostExchange(value = "instances", contentType = MediaType.APPLICATION_JSON_VALUE)
  void saveInstance(@RequestBody Object instance);

  static String getReason(Exception ex) {
    if (ex instanceof HttpStatusCodeException that) {
      return that.getResponseBodyAsString();
    }
    return ex.getMessage();
  }
}
