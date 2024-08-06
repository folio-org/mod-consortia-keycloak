package org.folio.consortia.utils;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.RandomStringGenerator;

public class HelperUtils {

  private HelperUtils() {}

  public static void checkIdenticalOrThrow(String firstString, String secondString, String errorMsg) {
    if (!StringUtils.equals(firstString, secondString)) {
      throw new IllegalArgumentException(errorMsg);
    }
  }

  public static String randomString(Integer noOfString) {
    RandomStringGenerator generator = new RandomStringGenerator.Builder().withinRange('a', 'z').build();
    return generator.generate(noOfString);
  }
}
