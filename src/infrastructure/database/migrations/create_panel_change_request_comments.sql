CREATE TABLE panel_change_request_comments (
    request_id BIGINT NOT NULL,
    commenter_email VARCHAR(255) NOT NULL,

    comment_text TEXT NOT NULL,

    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),

    CONSTRAINT fk_pcrc_request
        FOREIGN KEY (request_id)
        REFERENCES panel_change_requests(request_id)
        ON DELETE CASCADE,

    CONSTRAINT fk_pcrc_user
        FOREIGN KEY (commenter_email)
        REFERENCES users(email)
        ON UPDATE CASCADE
        ON DELETE RESTRICT
);