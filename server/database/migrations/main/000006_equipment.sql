-- TYPES
DO $$ BEGIN
    -- EQUIP_KIND
    IF to_regtype('equipment.KIND') IS NULL THEN
        CREATE TYPE equipment.KIND AS ENUM(
            'HEAD',
            'TOP',
            'BOTTOM',
            'FEET',
            'ARMS',
            'FINGER'
        );
    END IF;

    -- EQUIP_USE
    IF to_regtype('equipment.USE') IS NULL THEN
        CREATE TYPE equipment.USE AS ENUM(
            'HEAD',
            'TOP',
            'BOTTOM',
            'FEET',
            'ARMS',
            'LEFT_ARM',
            'RIGHT_ARM',
            'FINGER'
        );
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
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       username TEXT NOT NULL,
       is_equiped BOOL NOT NULL,
       equipment_name TEXT NOT NULL,
       use equipment.USE NOT NULL,
       kind equipment.KIND NOT NULL,
       CHECK (equipment.check_equipment_position_compatibility(use, kind)),
       FOREIGN KEY (name, username, e_mail) REFERENCES character.instance(name, username, e_mail),
       FOREIGN KEY (equipment_name, kind) REFERENCES equipment.instance(name, kind),
       PRIMARY KEY(name, username, e_mail, equipment_name)
);
