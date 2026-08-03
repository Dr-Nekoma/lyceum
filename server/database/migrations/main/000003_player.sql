DO $$ BEGIN
    -- Email
    IF to_regtype('player.email') IS NULL THEN
      CREATE DOMAIN player.email AS citext
      CHECK ( value ~ '^[a-zA-Z0-9.!#$%&''*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$' );
    END IF;
END $$;

CREATE TABLE IF NOT EXISTS player.record(
    username TEXT NOT NULL,
    -- TODO: for debug purposes only, do this properly later
    password TEXT NOT NULL,
    email player.email NOT NULL,
    -- We use this to help the Erlang backend and MNESIA, as 
    -- it is easier to store a single key and it is safer to
    -- associate it to PIDs, since we don't allow users to 
    -- change their account and character names.
    uid BIGINT GENERATED ALWAYS AS (
        ('x' || substring(encode(digest(username || email, 'sha256'), 'hex'), 1, 16))::bit(64)::bigint
    ) STORED,
    PRIMARY KEY(username, email)
);

CREATE INDEX IF NOT EXISTS idx_player_uid
ON player.record(uid)
INCLUDE (username, email);

-- =================================================
-- Live player sessions, as a temporal table.
--
-- This used to be a RAM-only mnesia table pinned to a single service
-- node, which is why there could only ever be one of them: a second
-- service node would have kept its own private copy and the two would
-- have disagreed about who was logged in. The invariant "a player has
-- at most one live session" was enforced structurally, by there being
-- exactly one `cache` gen_server to ask.
--
-- Removing that single process would normally downgrade the invariant
-- to a convention. PostgreSQL 18's `WITHOUT OVERLAPS` puts it back
-- where it cannot be argued with: the primary key is an exclusion
-- constraint, so two overlapping sessions for one player are rejected
-- by the database no matter which service node asked. Row locking
-- serialises the writers; this is what keeps them honest.
--
-- Rows are closed, never deleted, so the table doubles as login
-- history. An open session is `upper(valid_at) = 'infinity'`.
--
-- The upper bound is a real `'infinity'` rather than an unbounded (NULL)
-- one on purpose. "Still open" is then an ordinary value in the domain
-- instead of a sentinel: every row has a concrete period, `upper()`
-- never returns NULL, and the test for a live session is a two-valued
-- comparison rather than `IS NULL`. `valid_at @> clock_timestamp()`
-- says the same thing when that reads better.
--
-- `client_pid` is an Erlang term (`term_to_binary/1`) rather than text:
-- `pid_to_list/1` output is only meaningful inside the VM that produced
-- it, so a pid written by one service node would decode to a different
-- pid on another. The external term format carries the node and
-- creation, so it round-trips.
--
-- `owner_node` is the *logic* node running the player FSM, not the
-- frontend node holding the client connection. It is the reaping key: a
-- service node is never connected to a frontend node and so cannot
-- observe it going down, but it is always connected to the logic nodes
-- that call it.
--
-- NOTE: `FOR PORTION OF` did not ship in PostgreSQL 18, so closing a
-- period is a plain UPDATE of the range's upper bound.
--
-- Every bound here is `clock_timestamp()`, never `now()`. `now()` is
-- the *transaction's* start time, so a transaction that began earlier
-- but reached the player's lock later would close a period with an
-- upper bound below its own lower bound -- which Postgres rejects
-- outright ("range lower bound must be less than or equal to range
-- upper bound"). `clock_timestamp()` advances with real time, so
-- periods come out in the order the locks were actually taken.
-- =================================================


CREATE TABLE IF NOT EXISTS player.session(
    player_id BIGINT NOT NULL,
    username TEXT NOT NULL,
    email player.email NOT NULL,
    client_pid BYTEA NOT NULL,
    client_node TEXT NOT NULL,
    owner_node TEXT NOT NULL,
    valid_at TSTZRANGE NOT NULL DEFAULT tstzrange(clock_timestamp(), 'infinity', '[)'),
    PRIMARY KEY (player_id, valid_at WITHOUT OVERLAPS)
);

-- The reaper looks up open sessions by the logic node that owns them,
-- oldest first. Partial, because closed rows are history and are never
-- part of that question.
CREATE INDEX IF NOT EXISTS idx_player_session_open
ON player.session (owner_node, lower(valid_at))
WHERE upper(valid_at) = 'infinity';

-- 000002_roles.sql grants on tables that existed when it ran plus
-- default privileges for later ones; being explicit here means this
-- table is usable even if that migration is ever narrowed.
GRANT SELECT, INSERT, UPDATE, DELETE ON player.session TO application;
