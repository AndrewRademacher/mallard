-- #!test
-- name: "samplePerson",
-- description: "Test that a whole person can be inserted.";
SET search_path TO contact;

DO $$
DECLARE
    person_id bigint;
BEGIN
    INSERT INTO person (name_first, name_last) VALUES ('John', 'Doe') RETURNING id INTO person_id;
    INSERT INTO phone (owner_id, digits) VALUES (person_id, '444-444-4444');
END;
$$ LANGUAGE plpgsql;