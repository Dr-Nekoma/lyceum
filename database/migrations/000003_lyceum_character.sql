CREATE TABLE lyceum.user(
       username VARCHAR(32) NOT NULL,
       password TEXT NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       PRIMARY KEY(username, e_mail)
);

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

CREATE OR REPLACE TRIGGER trigger_character_upsert
INSTEAD OF INSERT ON lyceum.view_character
FOR EACH ROW EXECUTE FUNCTION lyceum.view_character_upsert();
