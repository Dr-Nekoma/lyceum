CREATE TABLE lyceum.user(
       username VARCHAR(32) NOT NULL,
       password TEXT NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$') UNIQUE,
       PRIMARY KEY(e_mail)
);

-- TODO: I'm getting a bunch of migration conflicts because
-- this crappy name is not UNIQUE.
CREATE TABLE lyceum.character(
       id SERIAL NOT NULL UNIQUE, 
       name VARCHAR(18) NOT NULL UNIQUE,
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

-- TODO: this is getting annoying af, I don't have the time to 
--       deal with these annoying uniqueness constraints everywhere!!!
--CREATE TABLE lyceum.player(
--       id SERIAL NOT NULL, 
--       name VARCHAR(18) NOT NULL,
--       level SMALLINT NOT NULL DEFAULT 1 CHECK (level >= 1),
--       mana SMALLINT NOT NULL DEFAULT 100,
--       health SMALLINT NOT NULL DEFAULT 100,
--       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$') UNIQUE,
--       username VARCHAR(32) NOT NULL,
--       FOREIGN KEY (e_mail, username) REFERENCES lyceum.user(e_mail, username),
--       FOREIGN KEY (id, name, level, mana, health) REFERENCES lyceum.character(id, name, level, mana, health),       
--       PRIMARY KEY (name, username, e_mail)
--);

-- HEALTH = CONSTITUTION + (99 - CONSTITUTION) / (1 + e^(50 - ENDURANCE) * 0.1) + (99 - CONSTITUTION) / (1 + e^(50 - STRENGTH) * 0.1)
-- MANA = SOMETHING SIMILAR HERE

CREATE TABLE lyceum.character_stats(
       name VARCHAR(18) NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$') UNIQUE,
       username VARCHAR(32) NOT NULL,
       constitution SMALLINT NOT NULL CHECK (constitution > 0 AND constitution <= 150),
       wisdom SMALLINT NOT NULL CHECK (wisdom > 0 AND wisdom <= 150),
       strength SMALLINT NOT NULL CHECK (strength > 0 AND strength <= 150),
       endurance SMALLINT NOT NULL CHECK (endurance > 0 AND endurance <= 150),
       intelligence SMALLINT NOT NULL CHECK (intelligence > 0 AND intelligence <= 150),
       faith SMALLINT NOT NULL CHECK (faith > 0 AND faith <= 150),
       mana SMALLINT NOT NULL CHECK (mana > 0),
       health SMALLINT NOT NULL CHECK (health > 0),
       FOREIGN KEY (name) REFERENCES lyceum.character(name),
       PRIMARY KEY(name, username, e_mail)
);

CREATE TABLE lyceum.character_position(
    name VARCHAR(18) NOT NULL UNIQUE,
    --TODO: Someone removed email from the character table
    --e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$') UNIQUE,
    username VARCHAR(32) NOT NULL,
    x_position SMALLINT NOT NULL,
    y_position SMALLINT NOT NULL,
    face_direction SMALLINT NOT NULL CHECK (face_direction >= 0 AND face_direction < 360),	
    map_name VARCHAR(64) NOT NULL,
    FOREIGN KEY (name) REFERENCES lyceum.character(name),
    FOREIGN KEY (map_name) REFERENCES lyceum.map(name),
    PRIMARY KEY(name, map_name)
);

CREATE TABLE lyceum.active_characters(
    name VARCHAR(18) NOT NULL,
    --TODO: Someone removed email from the character table
    --e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$') UNIQUE,
    --TODO: Someone also removed the username as well
    --username VARCHAR(32) NOT NULL,
    user_pid VARCHAR(50) NOT NULL UNIQUE,       
    FOREIGN KEY (name) REFERENCES lyceum.character(name),
    PRIMARY KEY(name)      
);

CREATE OR REPLACE VIEW lyceum.view_character AS
SELECT * FROM lyceum.character
NATURAL JOIN lyceum.character_stats
NATURAL JOIN lyceum.character_position;

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

CREATE OR REPLACE TRIGGER trigger_character_upsert
INSTEAD OF INSERT ON lyceum.view_character
FOR EACH ROW EXECUTE FUNCTION lyceum.view_character_upsert();
