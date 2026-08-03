-- Enable certain extensions
CREATE EXTENSION IF NOT EXISTS pg_stat_statements;
CREATE EXTENSION IF NOT EXISTS pgcrypto;
CREATE EXTENSION IF NOT EXISTS citext;

-- Lets a primary key mix scalar columns with a range column, which is
-- what PostgreSQL 18's `WITHOUT OVERLAPS` needs: the constraint becomes
-- a GiST exclusion constraint and the scalar halves need GiST opclasses.
-- Used by the temporal tables in the player, character and equipment
-- schemas.
CREATE EXTENSION IF NOT EXISTS btree_gist;

-- https://github.com/omnigres/omnigres
CREATE EXTENSION IF NOT EXISTS omni_id;
CREATE EXTENSION IF NOT EXISTS omni_seq;
CREATE EXTENSION IF NOT EXISTS omni_types;

-- Create Game's schemas
CREATE SCHEMA IF NOT EXISTS player;
CREATE SCHEMA IF NOT EXISTS character;
CREATE SCHEMA IF NOT EXISTS map;
CREATE SCHEMA IF NOT EXISTS equipment;
