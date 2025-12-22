## 1.8.0 - Unreleased

## 1.7.3 - Released (Sunflower R1 2025 Bug Fix)
### Bug Fixes
* [MODCONSKC-98](https://folio-org.atlassian.net/browse/MODCONSKC-98) - Sunflower Backport - Tenant is not updated to 'Complete' status when no staff users exist on the tenant
* [MODCONSKC-97](https://folio-org.atlassian.net/browse/MODCONSKC-97) - Sunflower Backport - Enhance the process of sharing roles containing special characters
* [MODCONSKC-96](https://folio-org.atlassian.net/browse/MODCONSKC-96) - Sunflower Backport - (ECS) Cannot share a role or save edits to a shared role

[Full Changelog](https://github.com/folio-org/mod-consortia/compare/v1.7.2...v1.7.3)

## 1.7.2 - Released (Sunflower R1 2025 Bug Fix)
### Bug Fixes
* [MODCONSKC-76](https://folio-org.atlassian.net/browse/MODCONSKC-76) - Use SECURE_STORE_ENV, not ENV, for secure store key

[Full Changelog](https://github.com/folio-org/mod-consortia/compare/v1.7.1...v1.7.2)

## 1.7.1 - Released (Sunflower R1 2025 Bug Fix)
The purpose of this release is to implement the relinking of identity provider links when usernames change.
Additionally, the shadow user's username is now updated after the real user's username is changed.
[Full Changelog](https://github.com/folio-org/mod-consortia/compare/v1.7.0...v1.7.1)

### Bug Fixes
* [MODCONSKC-73](https://folio-org.atlassian.net/browse/MODCONSKC-73) - Implement possibility for user to login with updated username

## 1.7.0 - Released (Sunflower R1 2025)
The purpose of this release is to implement sharing features and fixing module permissions
[Full Changelog](https://github.com/folio-org/mod-consortia/compare/v1.6.0...v1.7.0)

### Stories
* [MODCONSKC-56](https://folio-org.atlassian.net/browse/MODCONSKC-56) - Remove self endpoints invocations
* [MODCONSKC-57](https://folio-org.atlassian.net/browse/MODCONSKC-57) - Replace approach with using env varibaly for users module for custom-fields integration
* [MODCONSKC-61](https://folio-org.atlassian.net/browse/MODCONSKC-61) - ECS | Update Member tenant user email value on Central tenant
* [MODCONSKC-62](https://folio-org.atlassian.net/browse/MODCONSKC-62) - ECS - Improve Tenant Manager CRUD operations based on FSE feedback
* [MODCONSKC-63(https://folio-org.atlassian.net/browse/MODCONSKC-63) - Create an identity provider in keycloak when adding a tenant to the consortia
* [MODCONSKC-65](https://folio-org.atlassian.net/browse/MODCONSKC-65) - Custom Authentication Flow Configuration
* [MODCONSKC-68](https://folio-org.atlassian.net/browse/MODCONSKC-68) - PII in logs cleanup
* [MODCONSKC-69](https://folio-org.atlassian.net/browse/MODCONSKC-69) - Add support for deleting related module's data for consortia tenant hard delete
* [MODCONSKC-70](https://folio-org.atlassian.net/browse/MODCONSKC-70) - Create endpoints to add new Keycloak configuration for an existing consortia tenants
* [FOLIO-4204](https://folio-org.atlassian.net/browse/FOLIO-4204) - Update to mod-consortia Java 21


## 1.6.0 - Released (Ramsons R2 2024)
The purpose of this release is to implement sharing features and fixing module permissions
[Full Changelog](https://github.com/folio-org/mod-consortia/compare/v1.1.0...v1.2.0)

### Stories
* [MODCONSKC-45](https://folio-org.atlassian.net/browse/MODCONSKC-45) - Update libraries of dependant acq modules to the latest versions
* [MODCONSKC-42](https://folio-org.atlassian.net/browse/MODCONSKC-42) - Update module permissions in the ModuleDescriptor
* [MODCONSKC-39](https://folio-org.atlassian.net/browse/MODCONSKC-39) - Finish sharing role/policy implementations
* [MODCONSKC-37](https://folio-org.atlassian.net/browse/MODCONSKC-39) - Rename change-manager permissions
* [MODCONSKC-34](https://folio-org.atlassian.net/browse/MODCONSKC-34) - Add support to delete all capability sets/role associations for the shadow user & module cleanup
* [MODCONSKC-33](https://folio-org.atlassian.net/browse/MODCONSKC-33) - Change list of permissions sets
* [MODCONSKC-32](https://folio-org.atlassian.net/browse/MODCONSKC-32) - move custom field creation to save tenant operation
* [MODCONSKC-25](https://folio-org.atlassian.net/browse/MODCONSKC-25) - Sync mod-consortia merged PRs with mod-consortia-keycloak
* [MODCONSKC-21](https://folio-org.atlassian.net/browse/MODCONSKC-21) - Implement sharing of role capabilities
* [MODCONSKC-20](https://folio-org.atlassian.net/browse/MODCONSKC-20) - Implement sharing of role capability sets
* [MODCONSKC-19](https://folio-org.atlassian.net/browse/MODCONSKC-19) - Implement sharing of authorization roles
* [MODCONSKC-18](https://folio-org.atlassian.net/browse/MODCONSKC-18) - Implement sharing of authorization policies
* [MODCONSKC-7](https://folio-org.atlassian.net/browse/MODCONSKC-7) - adjust custom fields creation error handling
* [MODROLESKC-216](https://folio-org.atlassian.net/browse/MODROLESKC-216) - Make all enum values in endpoints response with upper case as stored in DB
* [MODCON-163](https://folio-org.atlassian.net/browse/MODCON-163) - Add system user permissions to support subject types/subject sources sharing
* [MODCON-159](https://folio-org.atlassian.net/browse/MODCON-159) - Make default limit param as 100 instead 10 all consortia endpoints
* [MODCON-158](https://folio-org.atlassian.net/browse/MODCON-158) - Extend "Inventory: Update ownership" permission
* [EUREKA-65](https://folio-org.atlassian.net/browse/EUREKA-65) - fix tenant data migration order to migrate user_tenant
* [UICONSET-203](https://folio-org.atlassian.net/browse/UICONSET-203) - Hide sharing action button in case when not all permissions are presented


## Version `v1.4.5` (25.09.2024)
### Changes:
* Moved custom field creation to save tenant operation (MODCONSKC-32)
* Implemented sharing of role capabilities (MODCONSKC-21)
* Adjusted custom fields creation error handling (MODCONSKC-7)
* No assigned roles\caps sets for consortium_admin(central) user account shadow entity in member tenant (MODCONSKC-33)
* Extended "Inventory: Update ownership" permission (MODCON-158)
* Made all enum values in endpoints response with upper case as stored in DB (MODROLESKC-216)
* Changed list of permissions sets (MODCONSKC-33)

## Version `v1.4.4` (30.08.2024)
### Changes:
* Sync mod-consortia merged PRs with mod-consortia-keycloak (MODCONSKC-25)

## Version `v1.4.3` (15.08.2024)
### Changes:
* Implement sharing of authorization roles (MODCONSKC-19)
* Аdd script to migrate data from mod-consortia (EUREKA-65)

## Version `v1.4.2` (14.08.2024)
### Changes:
* Upgrade application-poc-tools to v1.5.5.
* Implement sharing of authorization policies (MODCONSKC-18)
* Change system user type (EUREKA-225)

## Version `v1.4.1` (10.07.2024)
### Changes:
* Upgrade application-poc-tools to v1.5.4.

---
## Version `v1.4.0` (20.06.2024)
### Changes:
* Build Container Image for application ([RANCHER-1515](https://folio-org.atlassian.net/browse/RANCHER-1515)).

---
### Changes:
* Port Poppy CSP changes from mod-consortia -> mod-consortia-keycloak ([MODCONSKC-12](https://folio-org.atlassian.net/browse/MODCONSKC-12)).
* Add support of TLS when connecting to MSK ([MODCONSKC-10](https://folio-org.atlassian.net/browse/MODCONSKC-10)).

---
## Version `v1.2.0` (16.04.2024)
### Changes:
* Set updatedBy to the same value as createdBy in migration script and set update metadata upon record creation (MODCONSKC-11)

---
## Version `v1.1.1` (08.03.2024)
### Changes:
* Support for sidecar system user approach ([MODCONSKC-8](https://folio-org.atlassian.net/browse/MODCONSKC-8)).

---
## Version `v1.1.0` (27.02.2024)
### Changes:
* Added missing module permission ([MODCON-132](https://folio-org.atlassian.net/browse/MODCON-132)).
* Fixed user id extracting ([MODCONSKC-5](https://folio-org.atlassian.net/browse/MODCONSKC-5)).
