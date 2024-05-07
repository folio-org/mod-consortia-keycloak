package org.folio.consortia.domain.converter;

import org.folio.consortia.domain.dto.Tenant;
import org.folio.consortia.domain.entity.TenantDetailsEntity;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
public class TenantDetailsEntityToTenantConverter implements Converter<TenantDetailsEntity, Tenant> {
  @Override
  public Tenant convert(TenantDetailsEntity source) {
    Tenant tenant = new Tenant();
    tenant.setId(source.getId());
    tenant.setCode(source.getCode());
    tenant.setName(source.getName());
    tenant.setIsCentral(source.getIsCentral());
    tenant.setIsDeleted(source.getIsDeleted());
    return tenant;
  }
}
