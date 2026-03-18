package org.folio.consortia.client;

import org.folio.consortia.domain.dto.CustomField;
import org.folio.consortia.domain.dto.CustomFieldCollection;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.service.annotation.GetExchange;
import org.springframework.web.service.annotation.HttpExchange;
import org.springframework.web.service.annotation.PostExchange;

@HttpExchange("custom-fields")
public interface CustomFieldsClient {

  @GetExchange(accept = MediaType.APPLICATION_JSON_VALUE)
  CustomFieldCollection getByQuery(@RequestHeader(value = "x-okapi-module-id") String moduleId, @RequestParam("query") String query);

  @PostExchange(contentType = MediaType.APPLICATION_JSON_VALUE)
  void postCustomFields(@RequestHeader(value = "x-okapi-module-id") String moduleId, @RequestBody CustomField entity);
}
