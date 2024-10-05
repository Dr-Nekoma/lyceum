CREATE TYPE lyceum.SPELL_DESTRUCTION_TYPE AS ENUM(
       'MAGIC'
);

CREATE TYPE lyceum.SPELL_DAMAGE_TYPE AS ENUM(
       'FIRE',
       'PHYSICAL'
);

CREATE TYPE lyceum.SPELL_CONJURATION_TYPE AS ENUM(
       'FAKE WEAPONS',
       'INVOCATION'
);

CREATE TYPE lyceum.SPELL_HEAL_TYPE AS ENUM(
       'BLESSING'
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

CREATE TABLE lyceum.spell_destruction(
       name VARCHAR(16) NOT NULL,
       base_damage SMALLINT NOT NULL CHECK (base_damage > 0),
       damage_kind lyceum.SPELL_DAMAGE_TYPE NOT NULL,
       destruction_kind lyceum.SPELL_DESTRUCTION_TYPE NOT NULL,
       FOREIGN KEY (name) REFERENCES lyceum.spell(name)
);

CREATE TABLE lyceum.spell_conjuration(
       name VARCHAR(16) NOT NULL,
       conjuration_kind lyceum.SPELL_CONJURATION_TYPE NOT NULL,
       FOREIGN KEY (name) REFERENCES lyceum.spell(name)
);

CREATE TABLE lyceum.spell_restoration(
       name VARCHAR(16) NOT NULL,
       base_heal SMALLINT NOT NULL CHECK (base_heal > 0),
       restoration_kind lyceum.SPELL_HEAL_TYPE NOT NULL,
       FOREIGN KEY (name) REFERENCES lyceum.spell(name)
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

CREATE OR REPLACE TRIGGER trigger_spell_destruction_upsert
INSTEAD OF INSERT ON lyceum.view_spell_destruction
FOR EACH ROW EXECUTE FUNCTION lyceum.view_spell_destruction_upsert();

CREATE OR REPLACE TRIGGER trigger_spell_conjuration_upsert
INSTEAD OF INSERT ON lyceum.view_spell_conjuration
FOR EACH ROW EXECUTE FUNCTION lyceum.view_spell_conjuration_upsert();

CREATE OR REPLACE TRIGGER trigger_spell_restoration_upsert
INSTEAD OF INSERT ON lyceum.view_spell_restoration
FOR EACH ROW EXECUTE FUNCTION lyceum.view_spell_restoration_upsert();
