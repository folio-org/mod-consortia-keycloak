package org.folio.consortia.domain.converter;

import org.folio.consortia.domain.dto.Consortium;
import org.folio.consortia.domain.entity.ConsortiumEntity;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

@Component
public class ConsortiumConverter implements Converter<ConsortiumEntity, Consortium> {

  @Override
  public Consortium convert(ConsortiumEntity source) {
    Consortium consortium = new Consortium();
    consortium.setId(source.getId());
    consortium.setName(source.getName());
    return consortium;
  }
}
