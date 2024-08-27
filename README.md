# mod-consortia-keycloak

Copyright (C) 2023-2024 The Open Library Foundation

This software is distributed under the terms of the Apache License,
Version 2.0. See the file "[LICENSE](LICENSE)" for more information.

### Description
`mod-consortia-keycloak` an alternative implementation of `mod-consortia` intended to be used in conjunction with other keycloak modules (e.g. `mod-users-keycloak`, etc.).

## Table of Contents

- [Introduction](#introduction)
- [API information](#api-information)
- [Permissions](#permissions)
- [Installing and deployment](#installing-and-deployment)
  - [Compiling](#compiling)
  - [Running it](#running-it)
  - [Docker](#docker)
  - [Module descriptor](#module-descriptor)
  - [Environment variables](#environment-variables)

## Introduction

APIs for Consortia module.

## API information

Consortia API provides the following URLs:

| Method | URL                                                     | Permissions                           | Description                                                     |
|--------|---------------------------------------------------------|---------------------------------------|-----------------------------------------------------------------|
| GET    | /consortia/{consortiumId}/tenants                       | consortia.tenants.collection.get      | Gets list of tenants based on consortiumId                      |
| GET    | /consortia/{consortiumId}/user-tenants                  | consortia.user-tenants.collection.get | Gets list of user-tenants based on consortiumId                 |
| GET    | /consortia/{consortiumId}/user-tenants/{associationId}  | consortia.user-tenants.item.get       | Gets single user-tenant based on consortiumId and associationId |
| GET    | /consortia/{consortiumId}                               | consortia.consortium.item.get         | Gets single tenant based on consortiumId                        |
| GET    | /consortia                                              | consortia.consortium.collection.get   | Gets list of consortia                                          |
| POST   | /consortia                                              | consortia.consortium.item.post        | Inserts single consortium                                       |
| POST   | /consortia/{consortiumId}/tenants                       | consortia.tenants.item.post           | Inserts a single tenant based on consortiumId                   |
| PUT    | /consortia/{consortiumId}/tenants/{tenantId}            | consortia.tenants.item.put            | Update a single tenant name based on consortiumId and tenantId  |
| PUT    | /consortia/{consortiumId}                               | consortia.consortium.item.put         | Update consortium name based on consortiumId                    |

More detail about mod-consortia
 - API can be found on api-guide.md: [API Docs](/docs/api-guide.md).
 - Schema architecture can be found on Consortia wiki-page: [mod-consortia schema and ER diagram](https://wiki.folio.org/display/DD/Defining+Tenant+Schema+For+Consortia).

## Permissions

Institutional users should be granted the following permissions in order to use this Consortia API:
```shell
consortia.all
```

## Installing and deployment

### Compiling

Compile with
```shell
mvn clean install
```

### Running it

Run locally on listening port 8081 (default listening port):

Using Docker to run the local stand-alone instance:

```shell
DB_HOST=localhost DB_PORT=5432 DB_DATABASE=okapi_modules DB_USERNAME=folio_admin DB_PASSWORD=folio_admin \
   java -Dserver.port=8081 -jar target/mod-consortia-keycloak*.jar
```

### Docker

Build the docker container with:

```shell
docker build -t mod-consortia-keycloak .
```

### Module Descriptor

See the built `target/ModuleDescriptor.json` for the interfaces that this module
requires and provides, the permissions, and the additional module metadata.

### Environment variables

| Name                              |     Default value     | Description                                                                                                                                                |
|:----------------------------------|:---------------------:|:-----------------------------------------------------------------------------------------------------------------------------------------------------------|
| DB_HOST                           |       postgres        | Postgres hostname                                                                                                                                          |
| DB_PORT                           |         5432          | Postgres port                                                                                                                                              |
| DB_USERNAME                       |      folio_admin      | Postgres username                                                                                                                                          |
| DB_PASSWORD                       |           -           | Postgres username password                                                                                                                                 |
| DB_DATABASE                       |     okapi_modules     | Postgres database name                                                                                                                                     |
| KAFKA_HOST                        |         kafka         | Kafka broker hostname                                                                                                                                      |
| KAFKA_PORT                        |         9092          | Kafka broker port                                                                                                                                          |
| KAFKA_SECURITY_PROTOCOL           |       PLAINTEXT       | Kafka security protocol used to communicate with brokers (SSL or PLAINTEXT)                                                                                |
| KAFKA_SSL_KEYSTORE_LOCATION       |           -           | The location of the Kafka key store file. This is optional for client and can be used for two-way authentication for client.                               |
| KAFKA_SSL_KEYSTORE_PASSWORD       |           -           | The store password for the Kafka key store file. This is optional for client and only needed if 'ssl.keystore.location' is configured.                     |
| KAFKA_SSL_TRUSTSTORE_LOCATION     |           -           | The location of the Kafka trust store file.                                                                                                                |
| KAFKA_SSL_TRUSTSTORE_PASSWORD     |           -           | The password for the Kafka trust store file. If a password is not set, trust store file configured will still be used, but integrity checking is disabled. |
| ENV                               |         folio         | Logical name of the deployment, must be set if Kafka/Elasticsearch are shared for environments, `a-z (any case)`, `0-9`, `-`, `_` symbols only allowed     |
| MOD_USERS_ID                      |           -           | ID of `mod-users` module that is used for `custom-fields` interface                                                                                        |
| OKAPI_URL                         |  http://sidecar:8081  | Okapi url                                                                                                                                                  | |
| CUSTOM_FIELDS_RETRY_BACKOFF_DELAY |         5000          | Custom fields retry backoff delay creation in milliseconds.                                                                                                |
| CUSTOM_FIELDS_RETRY_MAX_RETRIES   |           5           | Custom fields retry max attempts creation in milliseconds.                                                                                                 |
