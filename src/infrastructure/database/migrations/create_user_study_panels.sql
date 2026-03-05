CREATE TABLE user_study_panels (
    email VARCHAR NOT NULL,
    study_id BIGINT NOT NULL,
    panel_payload JSONB,

    granted_at TIMESTAMPTZ NOT NULL,
    revoked_at TIMESTAMPTZ,

    granted_by VARCHAR,
    revoked_by VARCHAR,

    CONSTRAINT user_study_panels_pk 
        PRIMARY KEY (email, study_id),

    CONSTRAINT user_study_panels_email_fk
        FOREIGN KEY (email)
        REFERENCES users(email)
        ON UPDATE CASCADE
        ON DELETE RESTRICT,

    CONSTRAINT user_study_panels_study_fk
        FOREIGN KEY (study_id)
        REFERENCES studies(study_id)
        ON UPDATE CASCADE
        ON DELETE RESTRICT,

    CONSTRAINT user_study_panels_granted_by_fk
        FOREIGN KEY (granted_by)
        REFERENCES users(email)
        ON UPDATE CASCADE
        ON DELETE SET NULL,

    CONSTRAINT user_study_panels_revoked_by_fk
        FOREIGN KEY (revoked_by)
        REFERENCES users(email)
        ON UPDATE CASCADE
        ON DELETE SET NULL
);