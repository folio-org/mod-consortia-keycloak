package org.folio.consortia.config;

import org.folio.consortia.client.CapabilitySetsClient;
import org.folio.consortia.client.ConsortiaConfigurationClient;
import org.folio.consortia.client.CustomFieldsClient;
import org.folio.consortia.client.EurekaProxyTenantsClient;
import org.folio.consortia.client.InventoryClient;
import org.folio.consortia.client.PoliciesClient;
import org.folio.consortia.client.RoleCapabilitiesClient;
import org.folio.consortia.client.RoleCapabilitySetsClient;
import org.folio.consortia.client.RolesClient;
import org.folio.consortia.client.UserCapabilitiesClient;
import org.folio.consortia.client.UserCapabilitySetsClient;
import org.folio.consortia.client.UserRolesClient;
import org.folio.consortia.client.UserTenantsClient;
import org.folio.consortia.client.UsersClient;
import org.folio.consortia.client.UsersKeycloakClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.service.invoker.HttpServiceProxyFactory;

@Configuration
public class HttpClientConfiguration {

  @Bean
  public CapabilitySetsClient capabilitySetsClient(HttpServiceProxyFactory factory) {
    return factory.createClient(CapabilitySetsClient.class);
  }

  @Bean
  public ConsortiaConfigurationClient consortiaConfigurationClient(HttpServiceProxyFactory factory) {
    return factory.createClient(ConsortiaConfigurationClient.class);
  }

  @Bean
  public CustomFieldsClient customFieldsClient(HttpServiceProxyFactory factory) {
    return factory.createClient(CustomFieldsClient.class);
  }

  @Bean
  public EurekaProxyTenantsClient eurekaProxyTenantsClient(HttpServiceProxyFactory factory) {
    return factory.createClient(EurekaProxyTenantsClient.class);
  }

  @Bean
  public InventoryClient inventoryClient(HttpServiceProxyFactory factory) {
    return factory.createClient(InventoryClient.class);
  }

  @Bean
  public PoliciesClient policiesClient(HttpServiceProxyFactory factory) {
    return factory.createClient(PoliciesClient.class);
  }

  @Bean
  public RoleCapabilitiesClient roleCapabilitiesClient(HttpServiceProxyFactory factory) {
    return factory.createClient(RoleCapabilitiesClient.class);
  }

  @Bean
  public RoleCapabilitySetsClient roleCapabilitySetsClient(HttpServiceProxyFactory factory) {
    return factory.createClient(RoleCapabilitySetsClient.class);
  }

  @Bean
  public RolesClient rolesClient(HttpServiceProxyFactory factory) {
    return factory.createClient(RolesClient.class);
  }

  @Bean
  public UserCapabilitiesClient userCapabilitiesClient(HttpServiceProxyFactory factory) {
    return factory.createClient(UserCapabilitiesClient.class);
  }

  @Bean
  public UserCapabilitySetsClient userCapabilitySetsClient(HttpServiceProxyFactory factory) {
    return factory.createClient(UserCapabilitySetsClient.class);
  }

  @Bean
  public UserRolesClient userRolesClient(HttpServiceProxyFactory factory) {
    return factory.createClient(UserRolesClient.class);
  }

  @Bean
  public UserTenantsClient userTenantsClient(HttpServiceProxyFactory factory) {
    return factory.createClient(UserTenantsClient.class);
  }

  @Bean
  public UsersClient usersClient(HttpServiceProxyFactory factory) {
    return factory.createClient(UsersClient.class);
  }

  @Bean
  public UsersKeycloakClient usersKeycloakClient(HttpServiceProxyFactory factory) {
    return factory.createClient(UsersKeycloakClient.class);
  }
}

