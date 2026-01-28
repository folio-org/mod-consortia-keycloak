package org.folio.consortia.config.keycloak;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

import java.security.KeyStore;

import static org.folio.consortia.FolioConsortiaApplication.BCFKS_KEYSTORE_TYPE;
import static org.junit.jupiter.api.Assertions.*;

// Load all classes to test the provider
@SpringBootTest
class KeycloakFeignClientConfigTest {

  @Test
  void verifyBouncyCastleIsRegistered() throws Exception {
    assertNotNull(KeyStore.getInstance(BCFKS_KEYSTORE_TYPE));
  }
}
