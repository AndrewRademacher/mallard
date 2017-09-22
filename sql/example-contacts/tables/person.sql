-- #!migration
-- name: "tables/person",
-- description: "The root person table.",
-- requires: ["schema"];
SET search_path TO contact;

CREATE TABLE person(
    id          BIGSERIAL       NOT NULL,
    name_first  text,
    name_middle text,
    name_last   text,

    PRIMARY KEY (id)
);