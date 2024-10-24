DROP SCHEMA lyceum CASCADE;
DO $$
DECLARE
    sql_command RECORD;
BEGIN
    FOR sql_command IN
        SELECT 'TRUNCATE TABLE lyceum.' || table_name || ' RESTART IDENTITY CASCADE;' AS truncation_command
        FROM information_schema.tables as O
        WHERE table_schema = 'lyceum' and O.table_type <> 'VIEW'
    LOOP
        EXECUTE sql_command.truncation_command;
    END LOOP;
END $$;
TRUNCATE database_migrations_history RESTART IDENTITY;
