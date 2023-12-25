package org.folio.consortia.controller;

import static org.springframework.http.HttpStatus.NO_CONTENT;

import java.net.URI;
import java.util.UUID;

import org.folio.consortia.domain.dto.PublicationDetailsResponse;
import org.folio.consortia.domain.dto.PublicationRequest;
import org.folio.consortia.domain.dto.PublicationResponse;
import org.folio.consortia.domain.dto.PublicationResultCollection;
import org.folio.consortia.service.PublicationService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;

@RestController
@RequestMapping("/consortia/{consortiumId}")
@RequiredArgsConstructor
public class PublicationController implements org.folio.consortia.rest.resource.PublicationsApi {

  private final PublicationService publishCoordinatorService;

  @Override
  public ResponseEntity<PublicationResponse> publishRequests(UUID consortiumId, PublicationRequest publicationRequest) {
    PublicationResponse response = publishCoordinatorService.publishRequest(consortiumId, publicationRequest);
    return ResponseEntity.created(URI.create("/publications/" + response.getId())).body(response);
  }

  @Override
  public ResponseEntity<PublicationDetailsResponse> getPublicationDetails(UUID consortiumId, UUID publicationId){
    return ResponseEntity.ok(publishCoordinatorService.getPublicationDetails(consortiumId, publicationId));
  }

  @Override
  public ResponseEntity<PublicationResultCollection> getPublicationResults(UUID consortiumId, UUID publicationId) {
    return ResponseEntity.ok(publishCoordinatorService.getPublicationResults(consortiumId, publicationId));
  }

  @Override
  public ResponseEntity<Void> deletePublicationById(UUID consortiumId, UUID publicationId) {
    publishCoordinatorService.deletePublicationById(consortiumId, publicationId);
    return ResponseEntity.status(NO_CONTENT).build();
  }
}
