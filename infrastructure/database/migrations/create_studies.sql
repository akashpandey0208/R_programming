CREATE TABLE studies (
    study_id BIGSERIAL PRIMARY KEY,
    study_name_protocol_number VARCHAR(255) NOT NULL,
    protocol_admin VARCHAR(255) NOT NULL,
    
    CONSTRAINT fk_studies_protocol_admin
        FOREIGN KEY (protocol_admin)
        REFERENCES users(email)
        ON UPDATE CASCADE
        ON DELETE RESTRICT
);