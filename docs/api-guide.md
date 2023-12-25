# Mod Consortia API Guide

## Consortia API

### Endpoints

| Method | URL                       | Permissions                         | Description                                      |
|--------|---------------------------|-------------------------------------|--------------------------------------------------|
| GET    | /consortia                | consortia.consortium.collection.get | Return list of consortia with `limit`, `offset`  |
| POST   | /consortia                | consortia.consortium.item.post      | Return saved consortia                           |
| GET    | /consortia/{consortiumId} | consortia.consortium.item.get       | Return a consortium based on `consortiumId`      |
| PUT    | /consortia/{consortiumId} | consortia.consortium.item.put       | Return updated consortia based on `consortiumId` |

## Tenants API

### Endpoints

| METHOD | URL                 | Permission                       | DESCRIPTION                                   |
|--------|---------------------|----------------------------------|-----------------------------------------------|
| GET    | /tenants            | consortia.tenants.collection.get | Return list of tenants with `limit`, `offset` |
| POST   | /tenants            | consortia.tenants.item.post      | Return saved tenant                           |
| PUT    | /tenants/{tenantId} | consortia.tenants.item.put       | Return updated tenant based on `consortiumId` |
| DELETE | /tenants/{tenantId} | consortia.tenants.item.delete    | Delete tenant based on `consortiumId`         |

## User Tenants Associations API

### Endpoints

| METHOD | URL                                                    | Permission                            | DESCRIPTION                                                                                          |
|--------|--------------------------------------------------------|---------------------------------------|------------------------------------------------------------------------------------------------------|
| GET    | /consortia/{consortiumId}/user-tenants                 | consortia.user-tenants.collection.get | Return list of user tenant affiliations based on `userId`, `username`, `tenantId`, `limit`, `offset` |
| GET    | /consortia/{consortiumId}/user-tenants/{associationId} | consortia.user-tenants.item.get       | Return user-tenant affiliation with provided `associationId`                                         |
| POST   | /consortia/{consortiumId}/user-tenants                 | consortia.user-tenants.item.post      | Return saved user-tenant affiliation                                                                 |
| DELETE | /consortia/{consortiumId}/user-tenants                 | consortia.user-tenants.item.delete    | Delete user-tenant affiliation based on `userId` and `tenantId`                                      |

### Supported params options

| Option               | Example                                                             | Description                                           |
|----------------------|---------------------------------------------------------------------|-------------------------------------------------------|
| `userId`             | `id = "1bddbe67-fd27-436f-901e-9fa66fe4ad1d"`                       | Find user-tenant affiliations with ID (UUID)          |
| `username`           | `username = "testuser"`                                             | Find user-tenant affiliations with username           |
| `tenantId`, `userId` | `tenantId = "diku"`, `userId="bddbe67-fd27-436f-901e-9fa66fe4ad1d"` | Find user-tenant affiliation with userId and tenantId |
| `limit`, `offset`    | `limit = 10`, `offset = 0`                                          | Limit and offset for returned objects                 |

### Other resources

This module's [API documentation](https://dev.folio.org/reference/api/#mod-consortia).
