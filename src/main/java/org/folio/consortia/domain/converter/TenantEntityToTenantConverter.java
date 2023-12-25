package org.folio.consortia.domain.converter;

import org.folio.consortia.domain.dto.Tenant;
import org.folio.consortia.domain.entity.TenantEntity;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
public class TenantEntityToTenantConverter implements Converter<TenantEntity, Tenant> {

  @Override
  public Tenant convert(TenantEntity source) {
    Tenant tenant = new Tenant();
    tenant.setId(source.getId());
    tenant.setCode(source.getCode());
    tenant.setName(source.getName());
    tenant.setIsCentral(source.getIsCentral());
    return tenant;
  }
}
