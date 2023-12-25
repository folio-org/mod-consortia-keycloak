package org.folio.consortia.utils;

import org.folio.consortia.domain.dto.Payload;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.DeserializationFeature;

import java.util.Base64;

@Slf4j
public class TokenUtils {

  private static final String UNDEFINED_USER_NAME = "UNDEFINED_USER__";
  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper()
    .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);

  private TokenUtils() {}

  public static boolean isValid(String token) {
    if (StringUtils.isBlank(token)) {
      return false;
    }

    String[] tokenParts = token.split("\\.");

    if (tokenParts.length != 3) {
      return false;
    }

    String encodedPayload = tokenParts[1];
    byte[] decodedJsonBytes = Base64.getDecoder().decode(encodedPayload);
    String decodedJson = new String(decodedJsonBytes);

    try {
      Payload payload = OBJECT_MAPPER.readValue(decodedJson, Payload.class);

      return !payload.getSub().contains(UNDEFINED_USER_NAME);
    } catch (Exception e) {
      log.error("Could not parse the token", e);
      return false;
    }
  }

}
