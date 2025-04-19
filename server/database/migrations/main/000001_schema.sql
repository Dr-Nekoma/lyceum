-- Enable certain extensions
CREATE EXTENSION IF NOT EXISTS pg_stat_statements;

-- Create Game's schemas
CREATE SCHEMA IF NOT EXISTS player;
CREATE SCHEMA IF NOT EXISTS character;
CREATE SCHEMA IF NOT EXISTS map;
CREATE SCHEMA IF NOT EXISTS equipment;
