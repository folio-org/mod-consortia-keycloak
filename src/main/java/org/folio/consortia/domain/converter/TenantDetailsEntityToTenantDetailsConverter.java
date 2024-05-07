package org.folio.consortia.domain.converter;

import org.folio.consortia.domain.dto.TenantDetails;
import org.folio.consortia.domain.entity.TenantDetailsEntity;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
public class TenantDetailsEntityToTenantDetailsConverter implements Converter<TenantDetailsEntity, TenantDetails> {
  @Override
  public TenantDetails convert(TenantDetailsEntity source) {
    return new TenantDetails()
      .id(source.getId())
      .code(source.getCode())
      .name(source.getName())
      .isCentral(source.getIsCentral())
      .isDeleted(source.getIsDeleted())
      .setupStatus(source.getSetupStatus());
  }
}
