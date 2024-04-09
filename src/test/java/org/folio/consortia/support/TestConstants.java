package org.folio.consortia.support;

import java.util.UUID;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class TestConstants {

  public static final UUID USER_ID = UUID.fromString("e0c74b9a-8d4c-4c38-8716-9997c6f7d26e");
  public static final UUID CONSORTIUM_ID = UUID.fromString("7698e46-c3e3-11ed-afa1-0242ac120002");
  public static final UUID ACTION_ID = UUID.fromString("dcfc317b-0d7c-4334-8656-596105fa6c99");
  public static final UUID INSTANCE_ID = UUID.fromString("111841e3-e6fb-4191-8fd8-5674a5107c33");
  public static final String CENTRAL_TENANT_ID = "consortium";
}
