-- Enable UUID extension
CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE access_requests (
    request_id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
 
    full_name VARCHAR(150) NOT NULL,
    email VARCHAR(255) UNIQUE NOT NULL,
    job_title VARCHAR(120) NOT NULL,
    department VARCHAR(120) NOT NULL,
    organization VARCHAR(150) NOT NULL,
    phone_number VARCHAR(20) NOT NULL,
    office_location VARCHAR(120) NOT NULL,
    employment_type VARCHAR(300) NOT NULL,
 
    panels_requested JSONB NOT NULL,
 
    study_name_protocol_number VARCHAR(200) NOT NULL,
    sponsor VARCHAR(200) NOT NULL,

    therapeutic_areas VARCHAR(300) NOT NULL,
 
    status VARCHAR(20) 
        CHECK (status IN ('pending','approved','rejected', 'expired')) 
        DEFAULT 'pending',
 
    request_comments TEXT,
    admin_comments TEXT,
 
    created_on TIMESTAMPTZ DEFAULT now(),
    last_modified_on TIMESTAMPTZ,
    last_modified_by VARCHAR(255)
);