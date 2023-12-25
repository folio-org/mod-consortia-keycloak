package org.folio.consortia.service.impl;

import org.folio.consortia.exception.ResourceAlreadyExistException;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.ConsortiumRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.folio.consortia.domain.dto.Consortium;
import org.folio.consortia.domain.dto.ConsortiumCollection;
import org.folio.consortia.domain.entity.ConsortiumEntity;
import org.folio.consortia.service.ConsortiumService;
import org.folio.consortia.utils.HelperUtils;
import org.springframework.core.convert.ConversionService;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@Log4j2
@RequiredArgsConstructor
public class ConsortiumServiceImpl implements ConsortiumService {
  private static final String CONSORTIUM_IDS_NOT_MATCHED_ERROR_MSG = "Request body consortiumId and path param consortiumId should be identical";
  private static final String CONSORTIUM_RESOURCE_EXIST_MSG_TEMPLATE = "System can not have more than one consortium record";

  private final ConsortiumRepository repository;
  private final ConversionService converter;

  @Override
  public Consortium save(Consortium consortiumDto) {
    checkAnyConsortiumNotExistsOrThrow();
    ConsortiumEntity entity = new ConsortiumEntity();
    entity.setId(consortiumDto.getId());
    entity.setName(consortiumDto.getName());
    ConsortiumEntity consortiumEntity = repository.save(entity);
    return converter.convert(consortiumEntity, Consortium.class);
  }

  @Override
  public Consortium get(UUID consortiumId) {
    ConsortiumEntity entity = repository.findById(consortiumId)
      .orElseThrow(() -> new ResourceNotFoundException("consortiumId", String.valueOf(consortiumId)));
    return converter.convert(entity, Consortium.class);
  }

  @Override
  public Consortium update(UUID consortiumId, Consortium consortiumDto) {
    checkConsortiumExistsOrThrow(consortiumId);
    HelperUtils.checkIdenticalOrThrow(consortiumId.toString(), consortiumDto.getId().toString(), CONSORTIUM_IDS_NOT_MATCHED_ERROR_MSG);
    ConsortiumEntity entity = toEntity(consortiumDto);
    ConsortiumEntity consortiumEntity = repository.save(entity);
    return converter.convert(consortiumEntity, Consortium.class);
  }

  @Override
  public ConsortiumCollection getAll() {
    var result = new ConsortiumCollection();

    Page<ConsortiumEntity> consortiaPage = repository.findAll(PageRequest.of(0, 1));
    result.setConsortia(consortiaPage.stream().map(o -> converter.convert(o, Consortium.class)).toList());
    result.setTotalRecords((int) consortiaPage.getTotalElements());
    return result;
  }

  @Override
  public void checkConsortiumExistsOrThrow(UUID consortiumId) {
    if (!repository.existsById(consortiumId)) {
      throw new ResourceNotFoundException("consortiumId", String.valueOf(consortiumId));
    }
  }

  private void checkAnyConsortiumNotExistsOrThrow() {
    if (repository.count() > 0) {
      throw new ResourceAlreadyExistException(CONSORTIUM_RESOURCE_EXIST_MSG_TEMPLATE);
    }
  }

  private ConsortiumEntity toEntity(Consortium consortium) {
    ConsortiumEntity entity = new ConsortiumEntity();
    entity.setId(consortium.getId());
    entity.setName(consortium.getName());
    return entity;
  }
}
