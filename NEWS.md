## Version `v1.4.6` (26.10.2024)
### Changes:
* Fixed migration script for setup_status field (MODCONSKC-40)

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
