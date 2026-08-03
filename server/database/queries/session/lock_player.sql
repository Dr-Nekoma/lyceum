-- Serialises logins for one player across every service node.
--
-- The subselect is not decoration: pg_advisory_xact_lock returns void
-- and pgo cannot decode that type, so the call is wrapped in something
-- that yields a real column.
SELECT 1 AS locked
FROM (SELECT pg_advisory_xact_lock($1::BIGINT)) AS _lock
