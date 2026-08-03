-- TYPES
DO $$ BEGIN
    -- Equipment Kind
    IF to_regtype('equipment.KIND') IS NULL THEN
        CREATE DOMAIN equipment.KIND AS TEXT 
        CONSTRAINT CHECK_EQUIPMENT_KIND
        NOT NULL CHECK (VALUE IN (
            'HEAD',
            'TOP',
            'BOTTOM',
            'FEET',
            'ARMS',
            'FINGER'
        ));
    END IF;

    -- Equipment Usage
    IF to_regtype('equipment.USE') IS NULL THEN
        CREATE DOMAIN equipment.USE AS TEXT 
        CONSTRAINT CHECK_EQUIPMENT_USE
        NOT NULL CHECK (VALUE IN (
            'HEAD',
            'TOP',
            'BOTTOM',
            'FEET',
            'ARMS',
            'LEFT_ARM',
            'RIGHT_ARM',
            'FINGER'
        ));
    END IF;
END $$;

-- TABLES
CREATE TABLE IF NOT EXISTS equipment.instance(
    name TEXT NOT NULL,
    description TEXT NOT NULL,
    kind equipment.KIND NOT NULL,
    PRIMARY KEY(name, kind)
);

-- This is going to be used in the next table, as a constraint
CREATE OR REPLACE FUNCTION equipment.check_equipment_position_compatibility(use equipment.USE, kind equipment.KIND) RETURNS BOOL AS $$
BEGIN
    RETURN CASE 
        WHEN use::TEXT = kind::TEXT THEN true
        WHEN use = 'RIGHT_ARM' AND kind = 'ARMS' THEN true
        WHEN use = 'LEFT_ARM' AND kind = 'ARMS' THEN true
        ELSE false
    END;
END;
$$ LANGUAGE plpgsql;

-- =================================================
-- What a character owns.
--
-- Ownership only. Whether a piece is currently worn, and where, is a
-- fact about a period of time and lives in equipment.equipped below.
-- Splitting them is what lets an owned-but-unequipped item exist: a
-- single table with a boolean could say "not worn" but could not say
-- when it stopped being worn, and could not stop two items claiming one
-- slot.
-- =================================================
CREATE TABLE IF NOT EXISTS equipment.character(
    name TEXT NOT NULL,
    email player.email NOT NULL,
    username TEXT NOT NULL,
    equipment_name TEXT NOT NULL,
    kind equipment.KIND NOT NULL,
    FOREIGN KEY (name, username, email) REFERENCES character.instance(name, username, email),
    FOREIGN KEY (equipment_name, kind) REFERENCES equipment.instance(name, kind),
    PRIMARY KEY(name, username, email, equipment_name)
);

-- =================================================
-- What a character is wearing, and when they wore it.
--
-- Equipping opens a period, unequipping closes it, so the table carries
-- its own history: what was worn during a fight is answerable later
-- rather than overwritten. The present is `upper(valid_at) = 'infinity'`.
--
-- Two constraints, saying different things:
--
--   * the primary key -- one item cannot be worn twice at the same
--     instant, which is the direct analogue of the old row-per-item key;
--
--   * the temporal UNIQUE on `use` -- at most one item in a slot at any
--     instant. This is the invariant the old `is_equiped BOOL` could not
--     express at all: with a key of (character, equipment_name) nothing
--     stopped two helmets both being flagged as equipped, and the only
--     defence was application code remembering to check.
--
-- Bounds are `clock_timestamp()` and the open bound is a real
-- `'infinity'`, not an unbounded range: "still worn" is then a value in
-- the domain rather than a NULL sentinel.
-- =================================================
CREATE TABLE IF NOT EXISTS equipment.equipped(
    name TEXT NOT NULL,
    email player.email NOT NULL,
    username TEXT NOT NULL,
    equipment_name TEXT NOT NULL,
    use equipment.USE NOT NULL,
    kind equipment.KIND NOT NULL,
    valid_at TSTZRANGE NOT NULL DEFAULT tstzrange(clock_timestamp(), 'infinity', '[)'),
    CHECK (equipment.check_equipment_position_compatibility(use, kind)),
    FOREIGN KEY (name, username, email, equipment_name)
        REFERENCES equipment.character(name, username, email, equipment_name),
    PRIMARY KEY (name, username, email, equipment_name, valid_at WITHOUT OVERLAPS),
    UNIQUE (name, username, email, use, valid_at WITHOUT OVERLAPS)
);

CREATE INDEX IF NOT EXISTS idx_equipment_equipped_open
ON equipment.equipped (name, username, email)
WHERE upper(valid_at) = 'infinity';
