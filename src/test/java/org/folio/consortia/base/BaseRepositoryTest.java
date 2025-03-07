package org.folio.consortia.base;

import static org.springframework.test.annotation.DirtiesContext.ClassMode.AFTER_CLASS;

import lombok.extern.log4j.Log4j2;
import org.folio.consortia.config.FolioAuditorAware;
import org.folio.consortia.support.extension.EnablePostgresExtension;
import org.folio.spring.FolioExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.boot.test.autoconfigure.orm.jpa.TestEntityManager;
import org.springframework.context.annotation.Import;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.bean.override.mockito.MockitoBean;

@Log4j2
@DataJpaTest(properties = "spring.liquibase.parameters.tenantname=consortium")
@EnablePostgresExtension
@DirtiesContext(classMode = AFTER_CLASS)
@Import({FolioAuditorAware.class})
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
public abstract class BaseRepositoryTest {

  @Autowired
  protected TestEntityManager entityManager;
  @MockitoBean
  protected FolioExecutionContext folioExecutionContext;
}
