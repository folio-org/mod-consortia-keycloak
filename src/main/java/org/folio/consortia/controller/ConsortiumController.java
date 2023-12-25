package org.folio.consortia.controller;

import lombok.RequiredArgsConstructor;
import org.folio.consortia.domain.dto.Consortium;
import org.folio.consortia.domain.dto.ConsortiumCollection;
import org.folio.consortia.rest.resource.ConsortiaApi;
import org.folio.consortia.service.ConsortiumService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

import java.util.UUID;

@RestController
@RequiredArgsConstructor
public class ConsortiumController implements ConsortiaApi {
  @Autowired
  ConsortiumService consortiumService;

  @Override
  public ResponseEntity<Consortium> saveConsortium(Consortium consortium) {
    return ResponseEntity.status(HttpStatus.CREATED).body(consortiumService.save(consortium));
  }

  @Override
  public ResponseEntity<Consortium> getConsortium(UUID consortiumId) {
    return ResponseEntity.ok(consortiumService.get(consortiumId));
  }

  @Override
  public ResponseEntity<Consortium> updateConsortium(UUID consortiumId, Consortium consortium) {
    return ResponseEntity.ok(consortiumService.update(consortiumId, consortium));
  }

  @Override
  public ResponseEntity<ConsortiumCollection> getConsortiumCollection() {
    return ResponseEntity.ok(consortiumService.getAll());
  }
}
