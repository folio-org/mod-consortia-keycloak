package org.folio.consortia.repository;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static java.lang.String.format;
import static org.assertj.core.api.Assertions.assertThat;
import static org.folio.consortia.base.BaseIT.TENANT;
import static org.folio.consortia.base.BaseIT.TOKEN;
import static org.folio.consortia.base.BaseIT.asJsonString;
import static org.springframework.http.MediaType.APPLICATION_JSON;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.common.ClasspathFileSource;
import com.github.tomakehurst.wiremock.extension.responsetemplating.ResponseTemplateTransformer;
import com.github.tomakehurst.wiremock.extension.responsetemplating.TemplateEngine;
import java.util.ArrayList;
import java.util.List;
import lombok.SneakyThrows;
import org.folio.consortia.repository.ConsortiaMigrationTest.DockerPostgresDataSourceInitializer;
import org.folio.consortia.support.extension.EnableKafkaExtension;
import org.folio.spring.integration.XOkapiHeaders;
import org.folio.tenant.domain.dto.TenantAttributes;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.ApplicationContextInitializer;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpHeaders;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.kafka.test.context.EmbeddedKafka;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.annotation.DirtiesContext.ClassMode;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.support.TestPropertySourceUtils;
import org.springframework.test.util.TestSocketUtils;
import org.springframework.test.web.servlet.MockMvc;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Testcontainers;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@ContextConfiguration(initializers = DockerPostgresDataSourceInitializer.class)
@AutoConfigureMockMvc
@Testcontainers
@EmbeddedKafka
@EnableKafkaExtension
@DirtiesContext(classMode = ClassMode.AFTER_CLASS)
class ConsortiaMigrationTest {

  @Autowired
  protected MockMvc mockMvc;
  @Autowired
  private JdbcTemplate jdbcTemplate;

  protected static final int WIRE_MOCK_PORT = TestSocketUtils.findAvailableTcpPort();
  protected static WireMockServer wireMockServer;
  protected static PostgreSQLContainer<?> postgreDBContainer = new PostgreSQLContainer<>("postgres:12-alpine");

  static {
    postgreDBContainer.start();
  }

  @BeforeAll
  static void beforeAll() {
    wireMockServer = new WireMockServer(wireMockConfig().port(WIRE_MOCK_PORT)
      .extensions(new ResponseTemplateTransformer(
        TemplateEngine.defaultTemplateEngine(), true, new ClasspathFileSource("/"), new ArrayList<>())));
    wireMockServer.start();
  }

  @Test
  @Sql("classpath:/sql/consortia_data.sql")
  void create_positive_migrateConsortiaData() {
    setUpTenant(mockMvc);

    var ids = jdbcTemplate.queryForList(
      format("SELECT id FROM %s.%s", TENANT + "_mod_consortia_keycloak", "consortia_configuration"), String.class);
    assertThat(ids).hasSize(1);
    assertThat(ids.iterator().next()).isEqualTo("e2628d7d-059a-46a1-a5ea-10a5a37b1af2");
  }

  @AfterAll
  static void tearDown() {
    wireMockServer.stop();
  }

  @SneakyThrows
  static void setUpTenant(MockMvc mockMvc) {
    mockMvc.perform(post("/_/tenant").content(asJsonString(new TenantAttributes().moduleTo("mod-consortia-keycloak")))
      .headers(defaultHeaders())
      .contentType(APPLICATION_JSON)).andExpect(status().isNoContent());
  }

  static HttpHeaders defaultHeaders() {
    var httpHeaders = new HttpHeaders();
    httpHeaders.setContentType(APPLICATION_JSON);
    httpHeaders.put(XOkapiHeaders.TENANT, List.of(TENANT));
    httpHeaders.add(XOkapiHeaders.URL, wireMockServer.baseUrl());
    httpHeaders.add(XOkapiHeaders.TOKEN, TOKEN);
    httpHeaders.add(XOkapiHeaders.USER_ID, "7698e46-c3e3-11ed-afa1-0242ac120002");
    return httpHeaders;
  }

  static class DockerPostgresDataSourceInitializer implements
    ApplicationContextInitializer<ConfigurableApplicationContext> {

    @Override
    public void initialize(ConfigurableApplicationContext context) {
      TestPropertySourceUtils.addInlinedPropertiesToEnvironment(context,
        "spring.datasource.url=" + postgreDBContainer.getJdbcUrl(),
        "spring.datasource.username=" + postgreDBContainer.getUsername(),
        "spring.datasource.password=" + postgreDBContainer.getPassword());
    }
  }

  @DynamicPropertySource
  static void setFolioOkapiUrl(DynamicPropertyRegistry registry) {
    registry.add("folio.okapi-url", () -> "http://localhost:" + WIRE_MOCK_PORT);
  }
}
