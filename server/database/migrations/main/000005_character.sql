-- TYPES
DO $$ BEGIN
    -- Movement Type
    IF to_regtype('"character".STATE_TYPE') IS NULL THEN
        CREATE DOMAIN character.STATE_TYPE AS TEXT 
        CONSTRAINT CHECK_STATE_TYPE
        NOT NULL CHECK (VALUE IN (
            'collecting_resource',
            'idle',
            'walking'
        ));
    END IF;
END $$;

-- TABLES
CREATE TABLE IF NOT EXISTS character.instance(
    name TEXT NOT NULL,
    email player.email NOT NULL,
    username TEXT NOT NULL,
    FOREIGN KEY (email, username) REFERENCES player.record(email, username),
    PRIMARY KEY(name, username, email)
);

CREATE TABLE IF NOT EXISTS character.stats(
    name TEXT NOT NULL,
    email player.email NOT NULL,
    username TEXT NOT NULL,
    constitution SMALLINT NOT NULL CHECK (constitution > 0 AND constitution <= 150),
    wisdom SMALLINT NOT NULL CHECK (wisdom > 0 AND wisdom <= 150),
    strength SMALLINT NOT NULL CHECK (strength > 0 AND strength <= 150),
    endurance SMALLINT NOT NULL CHECK (endurance > 0 AND endurance <= 150),
    intelligence SMALLINT NOT NULL CHECK (intelligence > 0 AND intelligence <= 150),
    faith SMALLINT NOT NULL CHECK (faith > 0 AND faith <= 150),
    level SMALLINT NOT NULL CHECK (level > 0 AND level <= 1000),
    health_max SMALLINT NOT NULL CHECK (health_max > 0 and health_max <= 1000) DEFAULT 100,
    health SMALLINT NOT NULL CHECK (health > 0 AND health <= health_max),
    mana_max SMALLINT NOT NULL CHECK (mana_max > 0 and mana_max <= 1000) DEFAULT 100,
    mana SMALLINT NOT NULL CHECK (mana > 0 AND mana <= mana_max),
    FOREIGN KEY (name, username, email) REFERENCES character.instance(name, username, email),
    PRIMARY KEY(name, username, email)
);

CREATE TABLE IF NOT EXISTS character.position(
    name TEXT NOT NULL,
    email player.email NOT NULL,
    username TEXT NOT NULL,
    x_position REAL NOT NULL,
    -- TODO: Turn this back into integers when the time comes
    y_position REAL NOT NULL,
    x_velocity REAL NOT NULL DEFAULT 0,
    y_velocity REAL NOT NULL DEFAULT 0,
    -- TODO: Wtf is happening. Why can't this be namespaced? Look in equipment schema file bro xD
    state_type "character".STATE_TYPE NOT NULL DEFAULT 'idle', 
    face_direction SMALLINT NOT NULL CHECK (face_direction >= 0 AND face_direction < 360),	
    map_name TEXT NOT NULL,
    FOREIGN KEY (name, username, email) REFERENCES character.instance(name, username, email),
    FOREIGN KEY (map_name) REFERENCES map.instance(name),
    PRIMARY KEY(name, username, email)
);

-- =================================================
-- Which characters are in the world, and when they were.
--
-- This is a temporal table: activating a character opens a period and
-- deactivating closes it, rather than inserting and deleting a row. The
-- present is `upper(valid_at) = 'infinity'`, which is all the game asks for;
-- keeping the closed periods costs one column and turns playtime, "last
-- seen" and session length into ordinary queries instead of data nobody
-- has.
--
-- The primary key still says a character cannot be in the world twice.
-- `WITHOUT OVERLAPS` (PostgreSQL 18) just makes it say so about every
-- instant rather than only about now, and being an exclusion constraint
-- it is the database refusing, not the application remembering to check.
--
-- Bounds are `clock_timestamp()`, never `now()`: `now()` is the
-- transaction's start time, so a transaction that began earlier but
-- committed later would close a period below its own lower bound, which
-- Postgres rejects.
-- =================================================
CREATE TABLE IF NOT EXISTS character.active(
    name TEXT NOT NULL,
    email player.email NOT NULL,
    username TEXT NOT NULL,
    valid_at TSTZRANGE NOT NULL DEFAULT tstzrange(clock_timestamp(), 'infinity', '[)'),
    FOREIGN KEY (name, username, email) REFERENCES character.instance(name, username, email),
    PRIMARY KEY (name, username, email, valid_at WITHOUT OVERLAPS)
);

-- Every read is "who is in the world right now", so index exactly that.
CREATE INDEX IF NOT EXISTS idx_character_active_open
ON character.active (name, username, email)
WHERE upper(valid_at) = 'infinity';

CREATE TABLE IF NOT EXISTS character.item(
    name TEXT NOT NULL,
    description TEXT NOT NULL,
    weight SMALLINT NOT NULL,
    PRIMARY KEY(name)
);

CREATE TABLE IF NOT EXISTS character.inventory(
    name TEXT NOT NULL,
    email player.email NOT NULL,
    username TEXT NOT NULL,
    quantity SMALLINT NOT NULL,
    item_name TEXT NOT NULL,
    FOREIGN KEY (name, username, email) REFERENCES character.instance(name, username, email),
    FOREIGN KEY (item_name) REFERENCES character.item(name),       
    PRIMARY KEY (name, username, email, item_name)
);

