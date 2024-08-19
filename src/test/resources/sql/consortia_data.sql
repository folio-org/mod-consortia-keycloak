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
