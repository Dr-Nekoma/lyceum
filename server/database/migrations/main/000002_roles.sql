-- =================================================
-- Create an "application" user with access to READ
-- and WRITE SEQUENCES and TABLES.
-- =================================================
CREATE USER application WITH PASSWORD 'application' LOGIN;
GRANT CONNECT ON DATABASE lyceum TO application;

-- Grant application user access to all current schemas
DO $$
DECLARE
    schema_name TEXT;
BEGIN
    FOR schema_name IN SELECT nspname FROM pg_namespace WHERE nspname NOT IN ('information_schema', 'pg_catalog', 'pg_toast') LOOP
        EXECUTE format('GRANT USAGE ON SCHEMA %I TO application', schema_name);
        EXECUTE format('GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA %I TO application', schema_name);
        EXECUTE format('GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA %I TO application', schema_name);
        EXECUTE format('ALTER DEFAULT PRIVILEGES IN SCHEMA %I GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO application', schema_name);
        EXECUTE format('ALTER DEFAULT PRIVILEGES IN SCHEMA %I GRANT USAGE, SELECT ON SEQUENCES TO application', schema_name);
    END LOOP;
END
$$;

ALTER DEFAULT PRIVILEGES FOR ROLE application
GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO application;

ALTER DEFAULT PRIVILEGES FOR ROLE application
GRANT USAGE, SELECT ON SEQUENCES TO application;

-- =================================================
-- Create a "dispatcher" user with READ / WRITE to
-- SEQUENCES and TABLES.
-- =================================================
CREATE USER dispatcher WITH PASSWORD 'dispatcher' LOGIN;
GRANT CONNECT ON DATABASE lyceum TO dispatcher;

-- Grant dispatcher user access to all current schemas
DO $$
DECLARE
    schema_name TEXT;
BEGIN
    FOR schema_name IN SELECT nspname FROM pg_namespace WHERE nspname NOT IN ('information_schema', 'pg_catalog', 'pg_toast') LOOP
        EXECUTE format('GRANT USAGE ON SCHEMA %I TO dispatcher', schema_name);
        EXECUTE format('GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA %I TO dispatcher', schema_name);
        EXECUTE format('GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA %I TO dispatcher', schema_name);
        EXECUTE format('ALTER DEFAULT PRIVILEGES IN SCHEMA %I GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO dispatcher', schema_name);
        EXECUTE format('ALTER DEFAULT PRIVILEGES IN SCHEMA %I GRANT USAGE, SELECT ON SEQUENCES TO dispatcher', schema_name);
    END LOOP;
END
$$;

ALTER DEFAULT PRIVILEGES FOR ROLE dispatcher
GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO dispatcher;

ALTER DEFAULT PRIVILEGES FOR ROLE dispatcher
GRANT USAGE, SELECT ON SEQUENCES TO dispatcher;

-- =================================================
-- Create a "MNESIA" user with READ ONLY access
-- to all TABLES.
-- =================================================
CREATE USER mnesia WITH PASSWORD 'mnesia' LOGIN;
GRANT CONNECT ON DATABASE lyceum TO mnesia;

-- Grant mnesia user read-only access to all current
-- schemas
DO $$
DECLARE
    schema_name TEXT;
BEGIN
    FOR schema_name IN SELECT nspname FROM pg_namespace WHERE nspname NOT IN ('information_schema', 'pg_catalog', 'pg_toast') LOOP
        EXECUTE format('GRANT USAGE ON SCHEMA %I TO mnesia', schema_name);
        EXECUTE format('GRANT SELECT ON ALL TABLES IN SCHEMA %I TO mnesia', schema_name);
        EXECUTE format('GRANT USAGE ON ALL SEQUENCES IN SCHEMA %I TO mnesia', schema_name);
        EXECUTE format('ALTER DEFAULT PRIVILEGES IN SCHEMA %I GRANT SELECT ON TABLES TO mnesia', schema_name);
        EXECUTE format('ALTER DEFAULT PRIVILEGES IN SCHEMA %I GRANT USAGE ON SEQUENCES TO mnesia', schema_name);
    END LOOP;
END
$$;

ALTER DEFAULT PRIVILEGES FOR ROLE mnesia GRANT SELECT ON TABLES TO mnesia;
ALTER DEFAULT PRIVILEGES FOR ROLE mnesia GRANT USAGE ON SEQUENCES TO mnesia;
