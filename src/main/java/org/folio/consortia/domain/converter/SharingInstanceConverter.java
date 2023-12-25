package org.folio.consortia.domain.converter;

import org.folio.consortia.domain.entity.SharingInstanceEntity;
import java.util.Objects;

import org.folio.consortia.domain.dto.Metadata;
import org.folio.consortia.domain.dto.SharingInstance;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
public class SharingInstanceConverter implements Converter<SharingInstanceEntity, SharingInstance> {

  @Override
  public SharingInstance convert(SharingInstanceEntity source) {
    SharingInstance sharingInstance = new SharingInstance();
    sharingInstance.setId(source.getId());
    sharingInstance.setInstanceIdentifier(source.getInstanceId());
    sharingInstance.setSourceTenantId(source.getSourceTenantId());
    sharingInstance.setTargetTenantId(source.getTargetTenantId());
    sharingInstance.setStatus(source.getStatus());
    sharingInstance.setError(source.getError());
    Metadata metadata = new Metadata();
    metadata.setCreatedByUserId(source.getCreatedBy());
    // in order to prevent writing "null" as a string
    if (Objects.nonNull(source.getCreatedDate())) {
      metadata.setCreatedDate(String.valueOf(source.getCreatedDate()));
    }
    if (Objects.nonNull(source.getUpdatedDate())) {
      metadata.setUpdatedDate(String.valueOf(source.getUpdatedDate()));
    }
    metadata.setUpdatedByUserId(source.getUpdatedBy());
    sharingInstance.setMetadata(metadata);
    return sharingInstance;
  }
}
