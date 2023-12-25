package org.folio.consortia.service;

import org.folio.consortia.domain.dto.PublicationHttpResponse;
import org.springframework.http.HttpMethod;

public interface HttpRequestService {
  PublicationHttpResponse performRequest(String url, HttpMethod httpMethod, Object payload);
}
