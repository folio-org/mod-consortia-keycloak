package org.folio.consortia;

import jakarta.annotation.PostConstruct;
import lombok.extern.log4j.Log4j2;
import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.Security;

@Log4j2
@SpringBootApplication
public class FolioConsortiaApplication {

  public static final String BCFKS_KEYSTORE_TYPE = "BCFKS";

  static {
    Security.addProvider(new BouncyCastleProvider());
  }

  public static void main(String[] args) {
    SpringApplication.run(FolioConsortiaApplication.class, args);
  }

  @PostConstruct
  public void init() throws KeyStoreException {
    var instance = KeyStore.getInstance(BCFKS_KEYSTORE_TYPE);
    log.info("Loaded security provider: {}", instance.getProvider().getName());
  }
}
