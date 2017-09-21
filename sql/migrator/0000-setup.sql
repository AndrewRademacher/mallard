CREATE SCHEMA migration;

SET search_path TO migration;

CREATE TABLE migrator_version(
	version 		bigint   			NOT NULL,
	applied_on		timestamptz 		NOT NULL DEFAULT now(),
	
	PRIMARY KEY (version)
);