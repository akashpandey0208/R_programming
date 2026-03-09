CREATE TABLE employment_types (
    employment_id BIGSERIAL PRIMARY KEY,
    employment_type VARCHAR(300) UNIQUE NOT NULL,
    created_on TIMESTAMPTZ DEFAULT now()
);