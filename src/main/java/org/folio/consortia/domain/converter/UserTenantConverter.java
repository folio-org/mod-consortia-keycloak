package org.folio.consortia.domain.converter;

import org.folio.consortia.domain.dto.UserTenant;
import org.folio.consortia.domain.entity.UserTenantEntity;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.Objects;

@Component
public class UserTenantConverter implements Converter<UserTenantEntity, UserTenant> {

  @Override
  public UserTenant convert(UserTenantEntity source) {
    UserTenant userTenant = new UserTenant();
    userTenant.setId(source.getId());
    userTenant.setUserId(source.getUserId());
    userTenant.setUsername(source.getUsername());
    userTenant.setIsPrimary(source.getIsPrimary());
    if (Objects.nonNull(source.getTenant())) {
      userTenant.setTenantId(source.getTenant().getId());
      userTenant.setTenantName(source.getTenant().getName());
    }
    return userTenant;
  }

}
