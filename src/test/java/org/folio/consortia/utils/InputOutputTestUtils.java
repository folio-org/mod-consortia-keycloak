package org.folio.consortia.utils;

import org.folio.consortia.FolioConsortiaApplication;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.stream.Stream;

import org.apache.commons.io.IOUtils;

import com.fasterxml.jackson.databind.ObjectMapper;

import lombok.SneakyThrows;
import lombok.experimental.UtilityClass;
import lombok.extern.log4j.Log4j2;

@UtilityClass
@Log4j2
public class InputOutputTestUtils {

  @SneakyThrows
  public static String getMockDataAsString(String path) {

    try (InputStream resourceAsStream = FolioConsortiaApplication.class.getClassLoader().getResourceAsStream(path)) {
      if (resourceAsStream != null) {
        return IOUtils.toString(resourceAsStream, StandardCharsets.UTF_8);
      } else {
        StringBuilder sb = new StringBuilder();
        try (Stream<String> lines = Files.lines(Paths.get(path))) {
          lines.forEach(sb::append);
        }
        return sb.toString();
      }
    }
  }

  @SneakyThrows
  public static <T> T getMockDataObject(String path, Class<T> clazz) {
    var data = getMockDataAsString(path);
    return new ObjectMapper().readValue(data, clazz);
  }

  @SneakyThrows
  public static String writeValueAsString(Object object) {
    return new ObjectMapper().writeValueAsString(object);
  }
}
