CREATE SCHEMA IF NOT EXISTS lyceum;

CREATE TABLE lyceum.user(
       username VARCHAR(32) NOT NULL,
       password TEXT NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       PRIMARY KEY(username, e_mail)
);

CREATE TABLE lyceum.map(
       name VARCHAR(16) NOT NULL,
       PRIMARY KEY(name)
);

CREATE TYPE lyceum.TILE_TYPE AS ENUM(
       'WATER',
       'GRASS',
       'SAND',
       'ROCK'
);

CREATE TYPE lyceum.SPELL_TYPE AS ENUM(
       'PROJECTILE',
       'AREA',
       'BUFF'
);

CREATE TYPE lyceum.SPELL_TARGET AS ENUM(
       'SINGULAR',
       'VICINITY',
       'SELF'
);

CREATE TABLE lyceum.spell(
       name VARCHAR(16) NOT NULL,
       description VARCHAR(32) NOT NULL,
       cost SMALLINT NOT NULL CHECK (cost > 0),
       duration SMALLINT NOT NULL CHECK (duration >= 0),
       cast_time SMALLINT NOT NULL CHECK (cast_time >= 0),
       kind lyceum.SPELL_TYPE NOT NULL,
       target lyceum.SPELL_TARGET NOT NULL,
       PRIMARY KEY (name)
);

CREATE TYPE lyceum.SPELL_DESTRUCTION_TYPE AS ENUM(
       'MAGIC'
);

CREATE TYPE lyceum.SPELL_DAMAGE_TYPE AS ENUM(
       'FIRE',
       'PHYSICAL'
);

CREATE TABLE lyceum.spell_destruction(
       name VARCHAR(16) NOT NULL,
       base_damage SMALLINT NOT NULL CHECK (base_damage > 0),
       damage_kind lyceum.SPELL_DAMAGE_TYPE NOT NULL,
       destruction_kind lyceum.SPELL_DESTRUCTION_TYPE NOT NULL,
       FOREIGN KEY (name) REFERENCES lyceum.spell(name)
);

CREATE TYPE lyceum.SPELL_CONJURATION_TYPE AS ENUM(
       'FAKE WEAPONS',
       'INVOCATION'
);

CREATE TABLE lyceum.spell_conjuration(
       name VARCHAR(16) NOT NULL,
       conjuration_kind lyceum.SPELL_CONJURATION_TYPE NOT NULL,
       FOREIGN KEY (name) REFERENCES lyceum.spell(name)
);

CREATE TYPE lyceum.SPELL_HEAL_TYPE AS ENUM(
       'BLESSING'
);

CREATE TABLE lyceum.spell_restoration(
       name VARCHAR(16) NOT NULL,
       base_heal SMALLINT NOT NULL CHECK (base_heal > 0),
       restoration_kind lyceum.SPELL_HEAL_TYPE NOT NULL,
       FOREIGN KEY (name) REFERENCES lyceum.spell(name)
);

CREATE TABLE lyceum.tile(
       map_name VARCHAR(16) NOT NULL, 
       kind lyceum.TILE_TYPE NOT NULL,
       x_position SMALLINT NOT NULL,
       y_position SMALLINT NOT NULL,
       PRIMARY KEY(map_name, kind, x_position, y_position),
       FOREIGN KEY (map_name) REFERENCES lyceum.map(name)
);

CREATE TABLE lyceum.object(
       map_name VARCHAR(16) NOT NULL, 
       name VARCHAR(16) NOT NULL,
       x_position SMALLINT NOT NULL,
       y_position SMALLINT NOT NULL,
       PRIMARY KEY(map_name, x_position, y_position),
       FOREIGN KEY (map_name) REFERENCES lyceum.map(name)
);

-- Get rid of this table for instantenous spells, and do instead:
-- Make procedures for each name of spell, which will handle the entirety
-- of the side effects of that specific spell.
-- Things like target information come as arguments to the procedure
-- We gotta check if we can store procedures in tables. If yes, we need tagged unions,
-- If no, we may have to do the switching on Erlang.

-- Instance that are not instanteneous
CREATE TABLE lyceum.effect_instance(
       id         SERIAL NOT NULL,
       name       VARCHAR(16) NOT NULL,
       duration   SMALLINT NOT NULL CHECK (duration >= 0),
       map_name   VARCHAR(16) NOT NULL,
       owner_id   SERIAL NOT NULL,
       -- TODO: Targets can be something else, not only points
       x_position SMALLINT NOT NULL,
       y_position SMALLINT NOT NULL,       
       FOREIGN KEY (name) REFERENCES lyceum.spell(name),
       FOREIGN KEY (map_name) REFERENCES lyceum.map(name),
       FOREIGN KEY (owner_id) REFERENCES lyceum.map(name),
       FOREIGN KEY (map_name) REFERENCES lyceum.map(name),              
       PRIMARY KEY (id)
);

CREATE OR REPLACE FUNCTION effect_to_projectile() RETURNS trigger AS $effect_to_projectile$
DECLARE 
    spell_type TEXT;
    x_position SMALLINT;
    y_position SMALLINT;    
BEGIN
    SELECT kind INTO spell_type FROM lyceum.spell WHERE name = NEW.name;

    IF spell_type = 'PROJECTILE' THEN
        SELECT x_position, y_position INTO x_position, y_position
    	FROM lyceum.character
	WHERE id = NEW.owner_id;

       	INSERT INTO lyceum.projectile_instance (map_name, x_position, y_position, owner_id)
       	VALUES (NEW.map_name, x_position, y_position, NEW.owner_id);
    END IF;

    RETURN NEW;
END;
$effect_to_projectile$ LANGUAGE plpgsql;

CREATE TRIGGER effect_to_projectile BEFORE INSERT OR UPDATE ON lyceum.effect_instance
FOR EACH ROW EXECUTE FUNCTION effect_to_projectile();

-- - Do we need to record casted spells?
--   * Depends on duration (are we making `owner VARCHAR(32) NULL`?)
--   * Lifetime of spells may differ from their projectiles (if they are present)
-- - How will a casted spell interact with a projectile?
-- - How will we bind a projectile to an owner?
-- - How (and should we) will bind spells to an owner?
-- - How will NPCs cast spells? Yes, done below.

CREATE TYPE lyceum.PROJECTILE_PATHS AS ENUM(
       'LINEAR',
       'CURVE',
);

CREATE TABLE lyceum.projectile(
       spell_name VARCHAR(16) NOT NULL,
       initial_velocity FLOAT(4) NOT NULL,       
       duration SMALLINT NOT NULL CHECK (duration > 0),
       path_function lyceum.PROJECTILE_PATHS NOT NULL,
       PRIMARY KEY (spell_name),
       FOREIGN KEY (spell_name) REFERENCES lyceum.spell(name)       
);

CREATE TABLE lyceum.projectile_instance(
       id       SERIAL NOT NULL,
       map_name VARCHAR(16) NOT NULL,
       duration SMALLINT NOT NULL CHECK (duration > 0),       
       x_position SMALLINT NOT NULL,
       y_position SMALLINT NOT NULL,
       owner_id   SERIAL NOT NULL,
       PRIMARY KEY (id),
);

CREATE OR REPLACE FUNCTION map_object_overlap() RETURNS trigger AS $map_object_overlap$
DECLARE 
    kind TEXT;
BEGIN
    SELECT kind INTO kind FROM tile WHERE x_position = NEW.x_position AND y_position = NEW.y_position;

    IF NEW.name = 'TREE' THEN
        IF kind <> 'GRASS' AND kind <> 'SAND' THEN
            RAISE EXCEPTION '''TREE'' cannot be defined in tiles that are not ''GRASS'' or ''SAND''.';
        END IF;
    END IF;

    RETURN NEW;
END;
$map_object_overlap$ LANGUAGE plpgsql;

CREATE TRIGGER map_object_overlap BEFORE INSERT OR UPDATE ON lyceum.object
FOR EACH ROW EXECUTE FUNCTION map_object_overlap();

CREATE TABLE lyceum.character(
       id   SERIAL NOT NULL, 
       name VARCHAR(18) NOT NULL,
       level SMALLINT NOT NULL DEFAULT 1 CHECK (level >= 1),
       mana SMALLINT NOT NULL DEFAULT 100,
       health SMALLINT NOT NULL DEFAULT 100,
       PRIMARY KEY (id)	
);

-- CREATE TABLE lyceum.npc(
--        name VARCHAR(18) NOT NULL,
--        level SMALLINT NOT NULL DEFAULT 1 CHECK (level >= 1),
--        mana SMALLINT NOT NULL DEFAULT 100,
--        health SMALLINT NOT NULL DEFAULT 100,
-- );

CREATE TABLE lyceum.player(
       name VARCHAR(18) NOT NULL,
       level SMALLINT NOT NULL DEFAULT 1 CHECK (level >= 1),
       mana SMALLINT NOT NULL DEFAULT 100,
       health SMALLINT NOT NULL DEFAULT 100,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       username VARCHAR(32) NOT NULL,
       FOREIGN KEY (e_mail, username) REFERENCES lyceum.user(e_mail, username),
       FOREIGN KEY (name, level, mana, health) REFERENCES lyceum.character(name, level, mana, health),       
       PRIMARY KEY (name, username, e_mail)
);

-- HEALTH = CONSTITUTION + (99 - CONSTITUTION) / (1 + e^(50 - ENDURANCE) * 0.1) + (99 - CONSTITUTION) / (1 + e^(50 - STRENGTH) * 0.1)
-- MANA = SOMETHING SIMILAR HERE

CREATE TABLE lyceum.character_stats(
       name VARCHAR(18) NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       username VARCHAR(32) NOT NULL,
       constitution SMALLINT NOT NULL CHECK (constitution > 0 AND constitution <= 150),
       wisdom SMALLINT NOT NULL CHECK (wisdom > 0 AND wisdom <= 150),
       strength SMALLINT NOT NULL CHECK (strength > 0 AND strength <= 150),
       endurance SMALLINT NOT NULL CHECK (endurance > 0 AND endurance <= 150),
       intelligence SMALLINT NOT NULL CHECK (intelligence > 0 AND intelligence <= 150),
       faith SMALLINT NOT NULL CHECK (faith > 0 AND faith <= 150),
       mana SMALLINT NOT NULL CHECK (mana > 0),
       health SMALLINT NOT NULL CHECK (health > 0),
       FOREIGN KEY (name, username, e_mail) REFERENCES lyceum.character(name, username, e_mail),
       PRIMARY KEY(name, username, e_mail)
);

CREATE TABLE lyceum.character_position(
       name VARCHAR(18) NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       username VARCHAR(32) NOT NULL,
       x_position SMALLINT NOT NULL,
       y_position SMALLINT NOT NULL,
       face_direction SMALLINT NOT NULL CHECK (face_direction >= 0 AND face_direction < 360),	
       map_name VARCHAR(64) NOT NULL,
       FOREIGN KEY (name, username, e_mail) REFERENCES lyceum.character(name, username, e_mail),
       FOREIGN KEY (map_name) REFERENCES lyceum.map(name),
       PRIMARY KEY(name, username, e_mail)
);

CREATE TABLE lyceum.active_characters(
       name VARCHAR(18) NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       username VARCHAR(32) NOT NULL,
       user_pid VARCHAR(50) NOT NULL UNIQUE,       
       FOREIGN KEY (name, username, e_mail) REFERENCES lyceum.character(name, username, e_mail),
       PRIMARY KEY(name, username, e_mail)      
);

CREATE OR REPLACE VIEW lyceum.view_character AS
SELECT * FROM lyceum.character
NATURAL JOIN lyceum.character_stats
NATURAL JOIN lyceum.character_position;

CREATE OR REPLACE VIEW lyceum.view_spell_destruction AS
SELECT * FROM lyceum.spell
NATURAL JOIN lyceum.spell_destruction;

CREATE OR REPLACE VIEW lyceum.view_spell_conjuration AS
SELECT * FROM lyceum.spell
NATURAL JOIN lyceum.spell_conjuration;

CREATE OR REPLACE VIEW lyceum.view_spell_restoration AS
SELECT * FROM lyceum.spell
NATURAL JOIN lyceum.spell_restoration;


CREATE OR REPLACE FUNCTION lyceum.view_character_upsert() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
    INSERT INTO lyceum.character(name, e_mail, username)
    VALUES (NEW.name, NEW.e_mail, NEW.username)
    ON CONFLICT DO NOTHING;

    INSERT INTO lyceum.character_stats(name, e_mail, username, constitution, wisdom, strength, endurance, intelligence, faith)
    VALUES (NEW.name, NEW.e_mail, NEW.username, NEW.constitution, NEW.wisdom, NEW.strength, NEW.endurance, NEW.intelligence, NEW.faith)
    ON CONFLICT (name, e_mail, username) DO UPDATE SET
       name = NEW.name,
       e_mail = NEW.e_mail,
       username = NEW.username,
       constitution = NEW.constitution,
       wisdom = NEW.wisdom,
       strength = NEW.strength,
       endurance = NEW.endurance,
       intelligence = NEW.intelligence,
       faith = NEW.faith,
       face_direction = NEW.face_direction;
    -- Is this really necessary? The on conflict already catches this!
    -- WHERE name = NEW.name AND e_mail = NEW.e_mail AND username = NEW.username;

    INSERT INTO lyceum.character_position(name, e_mail, username, x_position, y_position, map_name)
    VALUES (NEW.name, NEW.e_mail, NEW.username, NEW.x_position, NEW.y_position, NEW.map_name)
    ON CONFLICT (name, username, e_mail) DO UPDATE SET
           name = NEW.name,
           e_mail = NEW.e_mail,
           username = NEW.username,
           x_position = NEW.x_position,
           y_position = NEW.y_position,
           map_name = NEW.map_name;

    -- Same issue here.
    -- WHERE name = NEW.name AND e_mail = NEW.e_mail AND username = NEW.username;

    RETURN NEW;
END
$$;


CREATE OR REPLACE FUNCTION lyceum.view_spell_destruction_upsert() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN      

    INSERT INTO lyceum.spell(name, description, cost, duration, cast_time, kind, target)
    VALUES (NEW.name, NEW.description, NEW.cost, NEW.duration, NEW.cast_time, NEW.kind, NEW.target)
    ON CONFLICT DO NOTHING;

    INSERT INTO lyceum.spell_destruction(name, base_damage, damage_kind, destruction_kind)
    VALUES (NEW.name, NEW.base_damage, NEW.damage_kind, NEW.destruction_kind)
    ON CONFLICT DO NOTHING;	       

    RETURN NEW;
END
$$;

CREATE OR REPLACE FUNCTION lyceum.view_spell_restoration_upsert() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN      

    INSERT INTO lyceum.spell(name, description, cost, duration, cast_time, kind, target)
    VALUES (NEW.name, NEW.description, NEW.cost, NEW.duration, NEW.cast_time, NEW.kind, NEW.target)
    ON CONFLICT DO NOTHING;

    INSERT INTO lyceum.spell_restoration(name, base_heal, restoration_kind)
    VALUES (NEW.name, NEW.base_heal, NEW.restoration_kind)
    ON CONFLICT DO NOTHING;	       

    RETURN NEW;
END
$$;

CREATE OR REPLACE FUNCTION lyceum.view_spell_conjuration_upsert() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN      

    INSERT INTO lyceum.spell(name, description, cost, duration, cast_time, kind, target)
    VALUES (NEW.name, NEW.description, NEW.cost, NEW.duration, NEW.cast_time, NEW.kind, NEW.target)
    ON CONFLICT DO NOTHING;

    INSERT INTO lyceum.spell_conjuration(name, conjuration_kind)
    VALUES (NEW.name, NEW.conjuration_kind)
    ON CONFLICT DO NOTHING;
    
    RETURN NEW;
END
$$;

CREATE TABLE lyceum.item(
       name VARCHAR(32) NOT NULL,
       description TEXT NOT NULL,
       PRIMARY KEY(name)
);

CREATE TABLE lyceum.character_inventory(
       name VARCHAR(18) NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       username VARCHAR(32) NOT NULL,
       quantity SMALLINT NOT NULL,
       item_name VARCHAR(32) NOT NULL,
       FOREIGN KEY (name, username, e_mail) REFERENCES lyceum.character(name, username, e_mail),
       FOREIGN KEY (item_name) REFERENCES lyceum.item(name),
       PRIMARY KEY(name, username, e_mail, item_name, quantity)
);

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
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       username VARCHAR(32) NOT NULL,
       is_equiped BOOL NOT NULL,
       equipment_name VARCHAR(32) NOT NULL,
       use lyceum.EQUIPMENT_USE NOT NULL,
       kind lyceum.EQUIPMENT_KIND NOT NULL,
       CHECK (lyceum.check_equipment_position_compatibility(use, kind)),
       FOREIGN KEY (name, username, e_mail) REFERENCES lyceum.character(name, username, e_mail),
       FOREIGN KEY (equipment_name, kind) REFERENCES lyceum.equipment(name, kind),
       PRIMARY KEY(name, username, e_mail, equipment_name)
);

CREATE OR REPLACE TRIGGER trigger_character_upsert
INSTEAD OF INSERT ON lyceum.view_character
FOR EACH ROW EXECUTE FUNCTION lyceum.view_character_upsert();

CREATE OR REPLACE TRIGGER trigger_spell_destruction_upsert
INSTEAD OF INSERT ON lyceum.view_spell_destruction
FOR EACH ROW EXECUTE FUNCTION lyceum.view_spell_destruction_upsert();

CREATE OR REPLACE TRIGGER trigger_spell_conjuration_upsert
INSTEAD OF INSERT ON lyceum.view_spell_conjuration
FOR EACH ROW EXECUTE FUNCTION lyceum.view_spell_conjuration_upsert();

CREATE OR REPLACE TRIGGER trigger_spell_restoration_upsert
INSTEAD OF INSERT ON lyceum.view_spell_restoration
FOR EACH ROW EXECUTE FUNCTION lyceum.view_spell_restoration_upsert();


-- INSERT INTO lyceum.user(username, e_mail, password)
-- VALUES ('test', 'test@email.com', '123');

-- INSERT INTO lyceum.view_character(name, e_mail, username, constitution, wisdom, strength, endurance, intelligence, faith, x_position, y_position, map_name)
-- VALUES ('knight', 'test@email.com', 'test', 10, 12, 13, 14, 15, 16, 0, 0, 'arda');

-- With map stuff
-- INSERT INTO lyceum.view_character(name, e_mail, username, constitution, wisdom, strength, endurance, intelligence, faith, x_position, y_position, map_name)
-- VALUES ('knight', 'test@email.com', 'test', 10, 12, 13, 14, 15, 16, 0, 0, 'CASTLE_HALL');
