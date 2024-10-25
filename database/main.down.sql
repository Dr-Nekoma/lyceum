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
TRUNCATE database_migrations_history RESTART IDENTITY;
