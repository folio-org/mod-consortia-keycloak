package org.folio.consortia;

import org.folio.common.configuration.properties.FolioEnvironment;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Import;

@SpringBootApplication
@Import(FolioEnvironment.class)
public class FolioConsortiaApplication {

  public static void main(String[] args) {
    SpringApplication.run(FolioConsortiaApplication.class, args);
  }
}
