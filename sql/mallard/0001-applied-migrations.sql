SET search_path TO mallard;

CREATE TABLE applied_migrations(
    id              bigserial       NOT NULL,
    name            text            NOT NULL,
    file_path       text            NOT NULL,
    description     text            NOT NULL,
    requires        text[]          NOT NULL,
    checksum        bytea           NOT NULL,
    script_text     text            NOT NULL,
    applied_on      timestamptz     NOT NULL DEFAULT now(),

    PRIMARY KEY (id)
);