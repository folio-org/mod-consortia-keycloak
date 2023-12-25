package org.folio.consortia.client;

import org.folio.spring.config.FeignClientConfiguration;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import com.fasterxml.jackson.databind.JsonNode;

import feign.FeignException;

@FeignClient(name = "inventory" , configuration = FeignClientConfiguration.class)
public interface InventoryClient {

  @GetMapping(value = "instances/{instanceId}")
  JsonNode getInstanceById(@PathVariable String instanceId);

  @PostMapping(value = "instances", consumes = MediaType.APPLICATION_JSON_VALUE)
  void saveInstance(@RequestBody Object instance);

  public static String getReason(Exception ex) {
    if (ex instanceof FeignException that) {
      return that.contentUTF8();
    }
    return ex.getMessage();
  }
}
