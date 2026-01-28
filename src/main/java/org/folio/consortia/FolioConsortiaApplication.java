package org.folio.consortia;

import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import java.security.Security;

@SpringBootApplication
public class FolioConsortiaApplication {

  public static final String BCFKS_KEYSTORE_TYPE = "BCFKS";

  static {
    Security.addProvider(new BouncyCastleProvider());
  }

  public static void main(String[] args) {
    SpringApplication.run(FolioConsortiaApplication.class, args);
  }
}
