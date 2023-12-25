package org.folio.consortia.domain.dto;

import org.springframework.http.HttpStatusCode;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class PublicationHttpResponse {
  private String body;
  private HttpStatusCode statusCode;
}
