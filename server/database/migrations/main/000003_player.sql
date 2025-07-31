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
