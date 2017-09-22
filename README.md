# Mallard

No frills migration for relational databases. Usable as either an application or library.

Mallard offers the following key features:

* Dependency resolution with circular reference prevention.
* Native migration scripts.
* Accretive implementation. Purposeful omission of "down migrations."

## Installation

There are currently no packaged distributions for Mallard, though they are coming soon. We were added to Stackage on Sep 21, 2017. So we just might make it into the LTS-9.6. Packaged distributions for Homebrew and Debian will be coming along soon. Currently, we don't know if there is any demand for Windows or RPM distribution, let us know if there is.

## Supported Databases

* PostgreSQL

## Application Mini-Manual

```
migrator - applies PSQL database migrations.

Usage: mallard ROOT [--host ARG] [--port ARG] [--user ARG] [--password ARG]
               --database ARG [-t|--test]
  Apply migrations to a database server.

Available options:
  --host ARG               Server host (default: "127.0.0.1")
  --port ARG               Server port (default: 5432)
  --user ARG               Username (default: "postgres")
  --password ARG           Password (default: "")
  --database ARG           Database name
  -t,--test                Run tests after migration.
  -h,--help                Show this help text
```

## Migration Structure

Mallard is designed to use a single root folder for all the migrations that define a database. Within this folder any structure is permitted. However, we recommend something like the following.

```
- root
    - tables
        - person.sql
        - phone.sql
    - views
        - person_with_phone.sql
    - functions
        - add_phone.sql
```

Wherein the migrations related to a given entity are kept in a single file. One such file might look like the following.

```sql
-- #!migration
-- name: "tables/phone",
-- description: "Phone numbers attached to a person.",
-- requires: ["tables/person"];
SET search_path TO contact;

CREATE TABLE phone(
    id          BIGSERIAL       NOT NULL,
    owner_id    bigint          NOT NULL,
    digits      text            NOT NULL,

    PRIMARY KEY (id),
    
    FOREIGN KEY (owner_id) REFERENCES person(id)
);

-- #!migration
-- name: "tables/phone/name",
-- description: "Add name column to phone number.",
-- requires: ["tables/phone"];
SET search_path TO contact;

ALTER TABLE phone ADD COLUMN name text;
```

Mallard is perfectly happy to accept as many migrations in a single file as you are willing to put there. However, there are a few rules you will have to follow.

* The SQL comments in which a migrations header are located must be contiguous.
* Each migration or test must begin with `#!migration` or `#!test`.
* Each header field must end with `,`, or if it is the last field a `;`.

These rules allow the header to be fully defined in the comments of an otherwise normal SQL file.