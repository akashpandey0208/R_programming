CREATE TABLE panel_change_requests (
    request_id BIGSERIAL PRIMARY KEY,

    requester_email VARCHAR NOT NULL,
    study_id BIGINT NOT NULL,

    panels_payload JSONB NOT NULL,

    panel_request_status VARCHAR NOT NULL
    CHECK (panel_request_status IN ('pending','primary_approver_approved','primary_approver_rejected', 'secondary_approver_approved', 'secondary_approver_rejected','expired')) 
    DEFAULT 'pending',

    requested_at TIMESTAMPTZ NOT NULL DEFAULT now(),

    primary_decision_by VARCHAR,
    secondary_decision_by VARCHAR,

    primary_decision_at TIMESTAMPTZ,
    secondary_decision_at TIMESTAMPTZ,

    requester_comment TEXT,
    primary_approver_comment TEXT,
    secondary_approver_comment TEXT,

    CONSTRAINT panel_change_requests_requester_fk
        FOREIGN KEY (requester_email)
        REFERENCES users(email)
        ON UPDATE CASCADE
        ON DELETE RESTRICT,

    CONSTRAINT panel_change_requests_study_fk
        FOREIGN KEY (study_id)
        REFERENCES studies(study_id)
        ON UPDATE CASCADE
        ON DELETE RESTRICT,

    CONSTRAINT panel_change_requests_primary_fk
        FOREIGN KEY (primary_decision_by)
        REFERENCES users(email)
        ON UPDATE CASCADE
        ON DELETE SET NULL,

    CONSTRAINT panel_change_requests_secondary_fk
        FOREIGN KEY (secondary_decision_by)
        REFERENCES users(email)
        ON UPDATE CASCADE
        ON DELETE SET NULL
);