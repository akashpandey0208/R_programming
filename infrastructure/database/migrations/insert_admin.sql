INSERT INTO organizations (organization_name, location)
VALUES ('Actalent Clinical Systems', 'Global HQ')
ON CONFLICT (organization_name) DO NOTHING;
 
INSERT INTO employment_types (employment_type)
VALUES ('System Administrator')
ON CONFLICT (employment_type) DO NOTHING;
 
INSERT INTO users (
    email,
    full_name,
    job_title,
    department,
    office_location,
    phone_number,
    organization_id,
    employment_id
)
VALUES (
    'admin@actalentsystems.com',
    'Jake Peralta',
    'System Administrator',
    'IT / Platform',
    'Global HQ',
    '+1-000-000-0000',
    (SELECT organization_id FROM organizations WHERE organization_name = 'Actalent Clinical Systems'),
    (SELECT employment_id FROM employment_types WHERE employment_type = 'System Administrator')
);
 
INSERT INTO user_login (
    email,
    password,
    is_reset,
    is_active
)
VALUES (
    'admin@actalentsystems.com',
    '$2b$12$jRbi611vf8r5hEExSLx5kuxXdxZQhtmVCYeUywBhsSUTf5xTxmx8K',
    TRUE,
    TRUE
);