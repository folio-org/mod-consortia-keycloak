package org.folio.consortia.controller;

import org.folio.consortia.service.CleanupService;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@RestController
@RequiredArgsConstructor
@Log4j2
public class TimerController {

  private final CleanupService cleanupService;

  @PostMapping(value = "/publications-cleanup")
  public void publicationsCleanup() {
    cleanupService.clearPublicationTables();
  }

}
