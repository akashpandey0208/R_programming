-- =========================
-- EMAIL LOGS
-- =========================

CREATE TABLE email_logs (
    log_id BIGSERIAL PRIMARY KEY,

    event_id BIGINT NOT NULL
        REFERENCES email_events(event_id)
        ON DELETE CASCADE,

    recipient_email VARCHAR(255) NOT NULL,
    -- populated by API
    sent_on TIMESTAMPTZ
);
