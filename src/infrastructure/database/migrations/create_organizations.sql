CREATE TABLE organizations (
    organization_id BIGSERIAL PRIMARY KEY,
    organization_name VARCHAR(150) UNIQUE NOT NULL,
    location VARCHAR(120),
    created_on TIMESTAMPTZ DEFAULT now()
);