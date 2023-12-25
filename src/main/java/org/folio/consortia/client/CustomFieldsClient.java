package org.folio.consortia.client;

import org.folio.consortia.domain.dto.CustomField;
import org.folio.consortia.domain.dto.CustomFieldCollection;
import org.folio.spring.config.FeignClientConfiguration;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "custom-fields", configuration = FeignClientConfiguration.class)
public interface CustomFieldsClient {

  @GetMapping(produces = MediaType.APPLICATION_JSON_VALUE)
  CustomFieldCollection getByQuery(@RequestHeader(value = "x-okapi-module-id") String moduleId, @RequestParam("query") String query);

  @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
  void postCustomFields(@RequestHeader(value = "x-okapi-module-id") String moduleId, @RequestBody CustomField entity);
}
