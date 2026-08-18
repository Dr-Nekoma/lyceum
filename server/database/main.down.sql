DROP SCHEMA IF EXISTS migraterl CASCADE;

-- omni_types:sum_type names the type it builds *literally*, so
-- 'map.ENTITY_TYPE' becomes a type called "map.ENTITY_TYPE" sitting in
-- public rather than a type called ENTITY_TYPE in map. Dropping the map
-- schema therefore leaves it behind, and the next db-up fails on a
-- duplicate pg_type name. Remove it here, registry row first, since
-- that row references the type's oid.
DO $$ BEGIN
    IF to_regtype('public."map.ENTITY_TYPE"') IS NOT NULL THEN
        DELETE FROM omni_types.sum_types
        WHERE typ = 'public."map.ENTITY_TYPE"'::regtype;

        DROP TYPE public."map.ENTITY_TYPE" CASCADE;
    END IF;
END $$;

DROP SCHEMA IF EXISTS player CASCADE;
DROP SCHEMA IF EXISTS map CASCADE;
DROP SCHEMA IF EXISTS character CASCADE;
DROP SCHEMA IF EXISTS equipment CASCADE;
DO $$
DECLARE
    sql_command RECORD;
BEGIN
    FOR sql_command IN
        SELECT 'TRUNCATE TABLE player.' || table_name || ' RESTART IDENTITY CASCADE;' AS truncation_command
        FROM information_schema.tables as O
        WHERE table_schema = 'player' and O.table_type <> 'VIEW'
    LOOP
        EXECUTE sql_command.truncation_command;
    END LOOP;
END $$;
DO $$
DECLARE
    sql_command RECORD;
BEGIN
    FOR sql_command IN
        SELECT 'TRUNCATE TABLE map.' || table_name || ' RESTART IDENTITY CASCADE;' AS truncation_command
        FROM information_schema.tables as O
        WHERE table_schema = 'map' and O.table_type <> 'VIEW'
    LOOP
        EXECUTE sql_command.truncation_command;
    END LOOP;
END $$;
DO $$
DECLARE
    sql_command RECORD;
BEGIN
    FOR sql_command IN
        SELECT 'TRUNCATE TABLE character.' || table_name || ' RESTART IDENTITY CASCADE;' AS truncation_command
        FROM information_schema.tables as O
        WHERE table_schema = 'character' and O.table_type <> 'VIEW'
    LOOP
        EXECUTE sql_command.truncation_command;
    END LOOP;
END $$;
DO $$
DECLARE
    sql_command RECORD;
BEGIN
    FOR sql_command IN
        SELECT 'TRUNCATE TABLE equipment.' || table_name || ' RESTART IDENTITY CASCADE;' AS truncation_command
        FROM information_schema.tables as O
        WHERE table_schema = 'equipment' and O.table_type <> 'VIEW'
    LOOP
        EXECUTE sql_command.truncation_command;
    END LOOP;
END $$;
