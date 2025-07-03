-- =================================================
-- Create a "migrations" superuser with permissions
-- to READ and WRITE on ROLES, SCHEMAS, SEQUENCES 
-- and TABLES.
-- =================================================
CREATE USER migrations
WITH LOGIN SUPERUSER CREATEROLE PASSWORD 'migrations';
GRANT CONNECT ON DATABASE lyceum TO migrations;
GRANT CREATE ON DATABASE lyceum TO migrations;

-- Grant default privileges for future schemas created by migrations user
ALTER DEFAULT PRIVILEGES FOR ROLE migrations GRANT ALL ON TABLES TO migrations;
ALTER DEFAULT PRIVILEGES FOR ROLE migrations GRANT ALL ON SEQUENCES TO migrations;

-- =================================================
-- Create an "admin" superuser as well
-- =================================================
CREATE USER admin
WITH LOGIN SUPERUSER CREATEROLE PASSWORD 'admin';
GRANT CONNECT ON DATABASE lyceum TO admin;
GRANT CREATE ON DATABASE lyceum TO admin;

ALTER DEFAULT PRIVILEGES FOR ROLE migrations GRANT ALL ON TABLES TO admin;
ALTER DEFAULT PRIVILEGES FOR ROLE migrations GRANT ALL ON SEQUENCES TO admin;
