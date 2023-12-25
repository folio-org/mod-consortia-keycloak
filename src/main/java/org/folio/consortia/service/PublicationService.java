package org.folio.consortia.service;

import java.util.UUID;

import org.folio.consortia.domain.dto.PublicationDetailsResponse;
import org.folio.consortia.domain.dto.PublicationRequest;
import org.folio.consortia.domain.dto.PublicationResponse;
import org.folio.consortia.domain.dto.PublicationResultCollection;

public interface PublicationService {
  PublicationResponse publishRequest(UUID consortiumId, PublicationRequest publication);

  PublicationDetailsResponse getPublicationDetails(UUID consortiumId, UUID publicationId);

  PublicationResultCollection getPublicationResults(UUID consortiumId, UUID publicationId);

  boolean checkPublicationDetailsExists(UUID consortiumId, UUID publicationId);

  void deletePublicationById(UUID consortiumId, UUID publicationId);
}
