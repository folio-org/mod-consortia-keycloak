package org.folio.consortia.service;

import org.folio.consortia.domain.entity.ConsortiumEntity;
import org.folio.consortia.exception.ResourceAlreadyExistException;
import org.folio.consortia.exception.ResourceNotFoundException;
import org.folio.consortia.repository.ConsortiumRepository;
import org.folio.consortia.service.impl.ConsortiumServiceImpl;
import org.folio.consortia.domain.dto.Consortium;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.batch.BatchAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.core.convert.ConversionService;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.folio.consortia.utils.EntityUtils.createConsortium;
import static org.folio.consortia.utils.EntityUtils.createConsortiumEntity;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@SpringBootTest
@EnableAutoConfiguration(exclude = BatchAutoConfiguration.class)
class ConsortiumServiceTest {

  @InjectMocks
  private ConsortiumServiceImpl consortiumService;
  @Mock
  private ConsortiumRepository consortiumRepository;
  @Mock
  private ConversionService conversionService;

  @Test
  void shouldGetConsortiumList() {
    ConsortiumEntity consortiumEntity = createConsortiumEntity("111841e3-e6fb-4191-8fd8-5674a5107c33", "Test");
    List<ConsortiumEntity> consortiumEntityList = new ArrayList<>();
    consortiumEntityList.add(consortiumEntity);

    when(consortiumRepository.findAll(PageRequest.of(0, 1)))
      .thenReturn(new PageImpl<>(consortiumEntityList, PageRequest.of(0, 1), consortiumEntityList.size()));

    var consortiumCollection = consortiumService.getAll();
    Assertions.assertEquals(1, consortiumCollection.getTotalRecords());
  }

  @Test
  void shouldSaveConsortium() {
    ConsortiumEntity consortiumEntity = createConsortiumEntity("111841e3-e6fb-4191-8fd8-5674a5107c33", "Test");
    Consortium consortium = createConsortium("111941e3-e6fb-4191-8fd8-5674a5107c33", "Test");
    when(consortiumRepository.save(any(ConsortiumEntity.class))).thenReturn(consortiumEntity);
    when(conversionService.convert(consortiumEntity, Consortium.class)).thenReturn(consortium);

    var consortium1 = consortiumService.save(consortium);
    Assertions.assertEquals(consortium, consortium1);
  }

  @Test
  void shouldGetErrorWhileSavingConsortium() {
    ConsortiumEntity consortiumEntity = createConsortiumEntity("111841e3-e6fb-4191-8fd8-5674a5107c33", "Test");
    Consortium consortium = createConsortium("111941e3-e6fb-4191-8fd8-5674a5107c33", "Test");
    when(consortiumRepository.count()).thenThrow(ResourceAlreadyExistException.class);
    when(conversionService.convert(consortiumEntity, Consortium.class)).thenReturn(consortium);

    Assertions.assertThrows(ResourceAlreadyExistException.class,
      () -> consortiumService.save(consortium));
  }

  @Test
  void shouldGetErrorWhileGetConsortium() {
    when(consortiumRepository.existsById(any())).thenReturn(false);
    Assertions.assertThrows(ResourceNotFoundException.class,
      () -> consortiumService.get(UUID.fromString("111941e3-e6fb-4191-8fd8-5674a5107c33")));
  }

  @Test
  void shouldGetErrorWhileGetConsortiumById() {
    when(consortiumRepository.existsById(any())).thenReturn(false);
    Assertions.assertThrows(ResourceNotFoundException.class,
      () -> consortiumService.checkConsortiumExistsOrThrow(UUID.fromString("111941e3-e6fb-4191-8fd8-5674a5107c33")));
  }

  @Test
  void shouldValidateConsortiumById() {
    UUID consortiumId = UUID.fromString("111941e3-e6fb-4191-8fd8-5674a5107c33");
    when(consortiumRepository.existsById(consortiumId)).thenReturn(true);
    Assertions.assertDoesNotThrow(() -> consortiumService.checkConsortiumExistsOrThrow(consortiumId));
  }

  @Test
  void shouldGetConsortium() {
    ConsortiumEntity consortiumEntity = createConsortiumEntity("111841e3-e6fb-4191-8fd8-5674a5107c33", "Test");
    Consortium consortium = createConsortium("111941e3-e6fb-4191-8fd8-5674a5107c33", "Test");
    when(consortiumRepository.findById(any())).thenReturn(Optional.of(consortiumEntity));
    when(conversionService.convert(consortiumEntity, Consortium.class)).thenReturn(consortium);

    var consortium1 = consortiumService.get(UUID.fromString("111941e3-e6fb-4191-8fd8-5674a5107c33"));
    Assertions.assertEquals(consortium, consortium1);
  }

  @Test
  void shouldUpdateConsortium() {
    ConsortiumEntity consortiumEntity = createConsortiumEntity("111841e3-e6fb-4191-8fd8-5674a5107c33", "Test");
    Consortium consortium = createConsortium("111941e3-e6fb-4191-8fd8-5674a5107c33", "Test1");
    when(consortiumRepository.existsById(any())).thenReturn(true);
    when(consortiumRepository.save(any())).thenReturn(consortiumEntity);
    when(conversionService.convert(consortiumEntity, Consortium.class)).thenReturn(consortium);

    var consortium1 = consortiumService.update(UUID.fromString("111941e3-e6fb-4191-8fd8-5674a5107c33"), consortium);
    Assertions.assertEquals(consortium.getId(), consortium1.getId());
    Assertions.assertEquals("Test1", consortium1.getName());
  }

  @Test
  void shouldThrowExceptionWhileUpdateTenant() {
    ConsortiumEntity consortiumEntity = createConsortiumEntity("111841e3-e6fb-4191-8fd8-5674a5107c33", "Test");
    Consortium consortium = createConsortium("111941e3-e6fb-4191-8fd8-5674a5107c33", "Test1");
    when(consortiumRepository.existsById(any())).thenReturn(true);
    when(conversionService.convert(consortiumEntity, Consortium.class)).thenReturn(consortium);

    Assertions.assertThrows(java.lang.IllegalArgumentException.class,
      () -> consortiumService.update(UUID.fromString("7698e46-c3e3-11ed-afa1-0242ac120002"), consortium));
  }

  @Test
  void shouldThrowNotFoundExceptionWhileUpdateTenant() {
    ConsortiumEntity consortiumEntity = createConsortiumEntity("111841e3-e6fb-4191-8fd8-5674a5107c33", "Test");
    Consortium consortium = createConsortium("111941e3-e6fb-4191-8fd8-5674a5107c33", "Test1");
    when(consortiumRepository.findById(any())).thenThrow(ResourceNotFoundException.class);
    when(conversionService.convert(consortiumEntity, Consortium.class)).thenReturn(consortium);

    Assertions.assertThrows(ResourceNotFoundException.class,
      () -> consortiumService.update(UUID.fromString("7698e46-c3e3-11ed-afa1-0242ac120002"), consortium));
  }

}
