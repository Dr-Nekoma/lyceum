-- Temporary schema, to nuke while testing changes
-- DROP SCHEMA IF EXISTS test_schema CASCADE;
CREATE SCHEMA IF NOT EXISTS test_schema;

-- https://docs.omnigres.org/omni_seq/id/#migration-guide
-- https://docs.omnigres.org/omni_id/identity_type/
-- https://yrashk.com/blog/2023/04/09/sum-types-in-postgres/
-- https://docs.omnigres.org/omni_types/sum_types/

CREATE EXTENSION IF NOT EXISTS omni_id;
CREATE EXTENSION IF NOT EXISTS omni_seq;
CREATE EXTENSION IF NOT EXISTS omni_types;
CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE DOMAIN CURRENCY AS TEXT 
CONSTRAINT CHECK_CURRENCY NOT NULL CHECK (VALUE IN ('dolar', 'real'));

-- TYPES
DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'test_schema.MOVEMENT_TYPE') THEN
        CREATE TYPE test_schema.MOVEMENT_TYPE AS ENUM(
            'idle',
            'walking',
            'running',
            'jumping',
            'falling',
            'flying'
        );
    END IF;

    IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'test_schema.ACTION_TYPE') THEN
        CREATE TYPE test_schema.ACTION_TYPE AS ENUM(
            'casting_spell',
            'collecting_resource'
        );
    END IF;

    -- Move this somewhere later
    IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'test_schema.email') THEN
        CREATE EXTENSION IF NOT EXISTS citext;
        CREATE DOMAIN test_schema.email AS citext
        CHECK ( value ~ '^[a-zA-Z0-9.!#$%&''*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$' );
    END IF;

END$$;

-- table omni_types.sum_types;
SELECT omni_types.sum_type('test_schema.STATE_TYPE', 'test_schema.MOVEMENT_TYPE', 'test_schema.ACTION_TYPE');

-- TABLES
-- o.g. player record
CREATE TABLE IF NOT EXISTS test_schema.record(
    username TEXT NOT NULL,
    password TEXT NOT NULL,
    email test_schema.email NOT NULL,
    PRIMARY KEY(username, email)
);


-- o.g. character instance
CREATE TABLE IF NOT EXISTS test_schema.instance(
    name TEXT NOT NULL,
    email test_schema.email NOT NULL,
    username TEXT NOT NULL,
    -- We use this to help the Erlang backend and MNESIA, as 
    -- it is easier to store a single key like. This is safe 
    -- to associate to PIDs in MNESIA sice we don't allow 
    -- users to change their account and character names.
    uid TEXT GENERATED ALWAYS AS (encode(digest(name || username, 'sha256'), 'hex')) STORED,
    FOREIGN KEY (email, username) REFERENCES test_schema.record(email, username),
    PRIMARY KEY(name, username, email)
);

-- TESTs
INSERT INTO test_schema.record(username, email, password) VALUES ('test', 'test@test.com', 'test');
INSERT INTO test_schema.instance(name, email, username) VALUES ('char', 'test@test.com', 'test');
