package org.folio.consortia.utils;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.RandomStringGenerator;

public class HelperUtils {

  public static final String CONSORTIUM_FOLIO_INSTANCE_SOURCE = "CONSORTIUM-FOLIO";
  public static final String CONSORTIUM_MARC_INSTANCE_SOURCE = "CONSORTIUM-MARC";
  public static final String CONSORTIUM_SETTING_SOURCE = "consortium";
  public static final String LOCAL_SETTING_SOURCE = "local";

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
