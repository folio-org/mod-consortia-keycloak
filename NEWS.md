## v1.7.0 - Unreleased

## 1.6.0 - Released (Ramsons R2 2024)
The purpose of this release is to implement sharing features and fixing module permissions
[Full Changelog](https://github.com/folio-org/mod-consortia/compare/v1.1.0...v1.2.0)

### Stories
* [MODCONSKC-45](https://folio-org.atlassian.net/browse/MODCONSKC-45) - Update libraries of dependant acq modules to the latest versions
* [MODCONSKC-44](https://folio-org.atlassian.net/browse/MODCONSKC-44) - Fix an issue related to broken release process (1.4.2-SNAPSHOT -> 1.5.0-SNAPSHOT -> 1.4.5-SNAPSHOT)
* [MODCONSKC-43](https://folio-org.atlassian.net/browse/MODCONSKC-43) - Release mod-consortia-keycloak v1.5.1 Ramsons Release (R2 2024)
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
* [EUREKASUP-8](https://folio-org.atlassian.net/browse/EUREKASUP-8) - fix tenant data migration order to migrate user_tenant
* [EUREKA-65](https://folio-org.atlassian.net/browse/EUREKA-65) - fix tenant data migration order to migrate user_tenant
* [UICONSET-203](https://folio-org.atlassian.net/browse/UICONSET-203) - Hide sharing action button in case when not all permissions are presented

### Bugfixes
* [MODCONSKC-40](https://folio-org.atlassian.net/browse/MODCONSKC-40) - DB migration issue


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
