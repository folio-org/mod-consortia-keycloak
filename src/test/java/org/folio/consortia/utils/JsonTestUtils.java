package org.folio.consortia.utils;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;

import lombok.SneakyThrows;
import lombok.experimental.UtilityClass;

@UtilityClass
public class JsonTestUtils {

  private static final JsonMapper MAPPER = JsonMapper.builder()
    .configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false)
    .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
    .serializationInclusion(JsonInclude.Include.NON_NULL)
    .addModule(new JavaTimeModule())
    .build();

  @SneakyThrows
  public static org.folio.consortia.domain.dto.UserEvent readUserEventMockFile(String filename) {
    return MAPPER.readValue(InputOutputTestUtils.getMockDataAsString(filename), org.folio.consortia.domain.dto.UserEvent.class);
  }

  @SneakyThrows
  public static <T> T getObjectFromJson(String json, Class<T> entityClass) {
    return MAPPER.readValue(json, entityClass);
  }

  @SneakyThrows
  public static <T> T getObjectFromJsonNode(JsonNode json, Class<T> entityClass) {
    return MAPPER.treeToValue(json, entityClass);
  }

  @SneakyThrows
  public static <T> T getMockAsObject(String mockPath, Class<T> entityClass) {
    return getObjectFromJson(InputOutputTestUtils.getMockDataAsString(mockPath), entityClass);
  }

  @SneakyThrows
  public static ObjectNode getMockAsJsonNode(String fullPath) {
    return (ObjectNode) MAPPER.readTree(InputOutputTestUtils.getMockDataAsString(fullPath));
  }

  @SneakyThrows
  public static String getObjectAsJson(Object o) {
    return MAPPER.writeValueAsString(o);
  }

  @SneakyThrows
  public static ObjectNode getObjectAsJsonNode(Object o) {
    return MAPPER.valueToTree(o);
  }
}
