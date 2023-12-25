package org.folio.consortia;

import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;

@SpringBootApplication
@EnableFeignClients
public class FolioConsortiaApplication {

  public static final String SYSTEM_USER_PASSWORD = "SYSTEM_USER_PASSWORD";

  public static void main(String[] args) {
    if (StringUtils.isEmpty(System.getenv(SYSTEM_USER_PASSWORD))) {
      throw new IllegalArgumentException("Required environment variable is missing: " + SYSTEM_USER_PASSWORD);
    }
    SpringApplication.run(FolioConsortiaApplication.class, args);
  }

}
