CREATE SCHEMA mallard;

SET search_path TO mallard;

CREATE TABLE migrator_version(
	version 		bigint   			NOT NULL,
	applied_on		timestamptz 		NOT NULL DEFAULT now(),
	
	PRIMARY KEY (version)
);