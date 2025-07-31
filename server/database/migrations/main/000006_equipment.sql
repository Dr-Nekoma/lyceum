-- TYPES
DO $$ BEGIN
    -- Equipment Kind
    IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'equipment.KIND') THEN
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
    IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'equipment.USE') THEN
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

CREATE TABLE IF NOT EXISTS equipment.character(
    name TEXT NOT NULL,
    email player.email NOT NULL,
    username TEXT NOT NULL,
    is_equiped BOOL NOT NULL,
    equipment_name TEXT NOT NULL,
    use equipment.USE NOT NULL,
    kind equipment.KIND NOT NULL,
    CHECK (equipment.check_equipment_position_compatibility(use, kind)),
    FOREIGN KEY (name, username, email) REFERENCES character.instance(name, username, email),
    FOREIGN KEY (equipment_name, kind) REFERENCES equipment.instance(name, kind),
    PRIMARY KEY(name, username, email, equipment_name)
);
