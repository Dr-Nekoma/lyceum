CREATE TYPE character.STATE_TYPE AS ENUM(
    'idle',
    'walking'
);

CREATE TABLE character.instance(
       name VARCHAR(18) NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       username VARCHAR(32) NOT NULL,
       FOREIGN KEY (e_mail, username) REFERENCES player.record(e_mail, username),
       PRIMARY KEY(name, username, e_mail)
);

CREATE TABLE character.stats(
       name VARCHAR(18) NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       username VARCHAR(32) NOT NULL,
       constitution SMALLINT NOT NULL CHECK (constitution > 0 AND constitution <= 150),
       wisdom SMALLINT NOT NULL CHECK (wisdom > 0 AND wisdom <= 150),
       strength SMALLINT NOT NULL CHECK (strength > 0 AND strength <= 150),
       endurance SMALLINT NOT NULL CHECK (endurance > 0 AND endurance <= 150),
       intelligence SMALLINT NOT NULL CHECK (intelligence > 0 AND intelligence <= 150),
       faith SMALLINT NOT NULL CHECK (faith > 0 AND faith <= 150),
       FOREIGN KEY (name, username, e_mail) REFERENCES character.instance(name, username, e_mail),
       PRIMARY KEY(name, username, e_mail)
);

CREATE TABLE character.position(
       name VARCHAR(18) NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       username VARCHAR(32) NOT NULL,
       x_position REAL NOT NULL,
       -- TODO: Turn this back into integers when the time comes
       y_position REAL NOT NULL,
       x_velocity REAL NOT NULL DEFAULT 0,
       y_velocity REAL NOT NULL DEFAULT 0,
       -- TODO: Wtf is happening. Why can't this be namespaced? Look in equipment schema file bro xD
       state_type STATE_TYPE NOT NULL DEFAULT 'idle', 
       face_direction SMALLINT NOT NULL CHECK (face_direction >= 0 AND face_direction < 360),	
       map_name VARCHAR(64) NOT NULL,
       FOREIGN KEY (name, username, e_mail) REFERENCES character.instance(name, username, e_mail),
       FOREIGN KEY (map_name) REFERENCES map.instance(name),
       PRIMARY KEY(name, username, e_mail)
);

CREATE TABLE character.active(
       name VARCHAR(18) NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       username VARCHAR(32) NOT NULL,
       user_pid VARCHAR(50) NOT NULL UNIQUE,       
       FOREIGN KEY (name, username, e_mail) REFERENCES character.instance(name, username, e_mail),
       PRIMARY KEY(name, username, e_mail)      
);

CREATE OR REPLACE VIEW character.view AS
SELECT * FROM character.instance
NATURAL JOIN character.stats
NATURAL JOIN character.position;

CREATE OR REPLACE FUNCTION character.view_upsert() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
    INSERT INTO character.instance(name, e_mail, username)
    VALUES (NEW.name, NEW.e_mail, NEW.username)
    ON CONFLICT DO NOTHING;

    INSERT INTO character.stats(name, e_mail, username, constitution, wisdom, strength, endurance, intelligence, faith)
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

    INSERT INTO character.position(name, e_mail, username, x_position, y_position, x_velocity, y_velocity, map_name, state_type)
    VALUES (NEW.name, NEW.e_mail, NEW.username, NEW.x_position, NEW.y_position, NEW.map_name, NEW.state_type)
    ON CONFLICT (name, username, e_mail) DO UPDATE SET
           name = NEW.name,
           e_mail = NEW.e_mail,
           username = NEW.username,
           x_position = NEW.x_position,
           y_position = NEW.y_position,
           x_position = NEW.x_position,
           y_position = NEW.y_position,	   
           state_type = NEW.state_type,
           map_name = NEW.map_name;

    RETURN NEW;
END
$$;

CREATE TABLE character.item(
       name VARCHAR(32) NOT NULL,
       description TEXT NOT NULL,
       PRIMARY KEY(name)
);

CREATE TABLE character.inventory(
       name VARCHAR(18) NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       username VARCHAR(32) NOT NULL,
       quantity SMALLINT NOT NULL,
       item_name VARCHAR(32) NOT NULL,
       FOREIGN KEY (name, username, e_mail) REFERENCES character.instance(name, username, e_mail),
       FOREIGN KEY (item_name) REFERENCES character.item(name),
       PRIMARY KEY(name, username, e_mail, item_name, quantity)
);

CREATE OR REPLACE TRIGGER trigger_character_upsert
INSTEAD OF INSERT ON character.view
FOR EACH ROW EXECUTE FUNCTION character.view_upsert();
