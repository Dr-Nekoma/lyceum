CREATE TYPE lyceum.EQUIPMENT_KIND AS ENUM(
       'HEAD',
       'TOP',
       'BOTTOM',
       'FEET',
       'ARMS',
       'FINGER'
);

CREATE TYPE lyceum.EQUIPMENT_USE AS ENUM(
       'HEAD',
       'TOP',
       'BOTTOM',
       'FEET',
       'ARMS',
       'LEFT_ARM',
       'RIGHT_ARM',
       'FINGER'
);

CREATE TABLE lyceum.item(
       name VARCHAR(32) NOT NULL,
       description TEXT NOT NULL,
       PRIMARY KEY(name)
);

CREATE TABLE lyceum.character_inventory(
       name VARCHAR(18) NOT NULL,
       --e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       --username VARCHAR(32) NOT NULL,
       quantity SMALLINT NOT NULL,
       item_name VARCHAR(32) NOT NULL,
       --FOREIGN KEY (name, username, e_mail) REFERENCES lyceum.character(name, username, e_mail),
       FOREIGN KEY (name) REFERENCES lyceum.character(name),
       FOREIGN KEY (item_name) REFERENCES lyceum.item(name),
       --PRIMARY KEY(name, username, e_mail, item_name, quantity)
       PRIMARY KEY(name, item_name, quantity)
);

CREATE TABLE lyceum.equipment(
       name VARCHAR(32) NOT NULL,
       description TEXT NOT NULL,
       kind lyceum.EQUIPMENT_KIND NOT NULL,
       PRIMARY KEY(name, kind)
);

CREATE OR REPLACE FUNCTION lyceum.check_equipment_position_compatibility(use lyceum.EQUIPMENT_USE, kind lyceum.EQUIPMENT_KIND) RETURNS BOOL AS $$
BEGIN
    RETURN CASE 
        WHEN use::TEXT = kind::TEXT THEN true
        WHEN use = 'RIGHT_ARM' AND kind = 'ARMS' THEN true
        WHEN use = 'LEFT_ARM' AND kind = 'ARMS' THEN true
        ELSE false
    END;
END;
$$ LANGUAGE plpgsql;

CREATE TABLE lyceum.character_equipment(
       name VARCHAR(18) NOT NULL,
       --e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       --username VARCHAR(32) NOT NULL,
       is_equiped BOOL NOT NULL,
       equipment_name VARCHAR(32) NOT NULL,
       use lyceum.EQUIPMENT_USE NOT NULL,
       kind lyceum.EQUIPMENT_KIND NOT NULL,
       CHECK (lyceum.check_equipment_position_compatibility(use, kind)),
       --FOREIGN KEY (name, username, e_mail) REFERENCES lyceum.character(name, username, e_mail),
       FOREIGN KEY (name) REFERENCES lyceum.character(name),
       FOREIGN KEY (equipment_name, kind) REFERENCES lyceum.equipment(name, kind),
       PRIMARY KEY(name, equipment_name)
);
