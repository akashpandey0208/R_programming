CREATE TABLE users (
    email VARCHAR(255) PRIMARY KEY,
    full_name VARCHAR(150) NOT NULL,
    job_title VARCHAR(120),
    department VARCHAR(120),
    phone_number VARCHAR(20),
 
    organization_id BIGINT,
    employment_id BIGINT,

    role_id INT NOT NULL,

    created_on TIMESTAMPTZ DEFAULT now(),
    updated_on TIMESTAMPTZ,
 
    CONSTRAINT fk_users_organization
        FOREIGN KEY (organization_id)
        REFERENCES organizations(organization_id)
        ON UPDATE CASCADE
        ON DELETE SET NULL,
 
    CONSTRAINT fk_users_employment
        FOREIGN KEY (employment_id)
        REFERENCES employment_types(employment_id)
        ON UPDATE CASCADE
        ON DELETE SET NULL,

    CONSTRAINT fk_access_requests_role
        FOREIGN KEY (role_id)
        REFERENCES roles(role_id)
        ON DELETE RESTRICT
);