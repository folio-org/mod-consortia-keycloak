package org.folio.consortia.service;

import org.folio.consortia.domain.entity.PublicationStatusEntity;
import org.folio.consortia.repository.PublicationStatusRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class PublicationStorageService {

  private final PublicationStatusRepository publicationStatusRepository;

  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public PublicationStatusEntity savePublicationStatusEntity(PublicationStatusEntity publicationStatusEntity) {
    return publicationStatusRepository.save(publicationStatusEntity);
  }
}
