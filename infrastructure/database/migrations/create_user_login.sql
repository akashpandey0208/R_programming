CREATE TABLE user_login (
    email VARCHAR(255) PRIMARY KEY,
    password TEXT NOT NULL,
 
    is_reset_required BOOLEAN DEFAULT TRUE,
    is_active BOOLEAN DEFAULT TRUE,
 
    last_login TIMESTAMPTZ,
    created_on TIMESTAMPTZ DEFAULT now(),
    updated_on TIMESTAMPTZ,
 
    CONSTRAINT fk_user_login_user
        FOREIGN KEY (email)
        REFERENCES users(email)
        ON DELETE CASCADE
        ON UPDATE CASCADE
);