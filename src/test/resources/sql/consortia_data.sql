CREATE SCHEMA if NOT EXISTS consortium_mod_consortia;

CREATE TABLE consortium_mod_consortia.consortia_configuration (
  id UUID PRIMARY KEY,
  central_tenant_id TEXT NOT NULL UNIQUE,
  created_by UUID,
  created_date TIMESTAMP WITHOUT TIME ZONE DEFAULT now() NOT NULL,
  updated_by UUID,
  updated_date TIMESTAMP WITHOUT TIME ZONE
);
INSERT INTO consortium_mod_consortia.consortia_configuration (id, central_tenant_id)
VALUES ('e2628d7d-059a-46a1-a5ea-10a5a37b1af2', 'text');

CREATE TABLE consortium_mod_consortia.consortium (
  id UUID PRIMARY KEY,
  name text NOT NULL UNIQUE,
  created_by UUID,
  created_date TIMESTAMP WITHOUT TIME ZONE DEFAULT now() NOT NULL,
  updated_by UUID,
  updated_date TIMESTAMP WITHOUT TIME ZONE
);
INSERT INTO consortium_mod_consortia.consortium (id, name)
VALUES ('88888888-8888-4888-8888-888888888888', 'text');

CREATE TYPE setup_status as ENUM ('IN_PROGRESS', 'COMPLETED', 'COMPLETED_WITH_ERRORS', 'FAILED');
CREATE CAST (character varying as setup_status) WITH INOUT AS IMPLICIT;

CREATE TABLE consortium_mod_consortia.tenant (
  id UUID PRIMARY KEY,
  name text NOT NULL UNIQUE,
  consortium_id UUID NOT NULL,
  code text NOT NULL UNIQUE,
  is_central boolean,
  created_by UUID,
  created_date TIMESTAMP WITHOUT TIME ZONE DEFAULT now() NOT NULL,
  updated_by UUID,
  updated_date TIMESTAMP WITHOUT TIME ZONE,
  setup_status setup_status NOT NULL,
  is_deleted boolean NOT NULL
);
INSERT INTO consortium_mod_consortia.tenant (id, name, consortium_id, code, is_central, setup_status, is_deleted)
VALUES ('e2628d7d-059a-46a1-a5ea-10a5a37b1af2', 'name', '88888888-8888-4888-8888-888888888888', 'code', false, 'IN_PROGRESS', false);
