package org.folio.consortia.utils;

import tools.jackson.databind.DeserializationFeature;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.json.JsonMapper;
import java.util.Base64;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.folio.consortia.domain.dto.Payload;
import org.folio.consortia.exception.InvalidTokenException;

@Slf4j
public class TokenUtils {

  private static final String UNDEFINED_USER_NAME = "UNDEFINED_USER__";
  private static final ObjectMapper OBJECT_MAPPER = JsonMapper.builder()
    .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
    .build();

  private TokenUtils() {}

  public static Payload validateAndParseTokenPayload(String token) {
    if (StringUtils.isBlank(token)) {
      throw new InvalidTokenException();
    }

    String[] tokenParts = token.split("\\.");

    if (tokenParts.length != 3) {
      throw new InvalidTokenException();
    }

    String encodedPayload = tokenParts[1];
    byte[] decodedJsonBytes = Base64.getDecoder().decode(encodedPayload);
    String decodedJson = new String(decodedJsonBytes);

    try {
      Payload payload = OBJECT_MAPPER.readValue(decodedJson, Payload.class);

      if (payload.getSub().contains(UNDEFINED_USER_NAME)) {
        throw new InvalidTokenException();
      }
      return payload;
    } catch (Exception e) {
      log.error("Could not parse the token", e);
      throw new InvalidTokenException();
    }
  }
}
