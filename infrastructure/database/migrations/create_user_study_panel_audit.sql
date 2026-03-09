CREATE TABLE user_study_panel_audit (
    audit_id BIGSERIAL PRIMARY KEY,

    email VARCHAR(255) NOT NULL,
    study_id BIGINT NOT NULL,
    panel_name VARCHAR(150) NOT NULL,

    granted_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    granted_by VARCHAR(255) NOT NULL,

    revoked_at TIMESTAMPTZ NULL,
    revoked_by VARCHAR(255) NULL,

    CONSTRAINT fk_audit_user
        FOREIGN KEY (email)
        REFERENCES users(email)
        ON DELETE CASCADE,

    CONSTRAINT fk_audit_study
        FOREIGN KEY (study_id)
        REFERENCES studies(study_id)
        ON DELETE CASCADE,

    CONSTRAINT fk_audit_granted_by
        FOREIGN KEY (granted_by)
        REFERENCES users(email),

    CONSTRAINT fk_audit_revoked_by
        FOREIGN KEY (revoked_by)
        REFERENCES users(email)
);