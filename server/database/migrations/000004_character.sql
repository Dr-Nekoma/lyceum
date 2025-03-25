DROP TYPE IF EXISTS "character.STATE_TYPE";
CREATE TYPE character.STATE_TYPE AS ENUM(
    'idle',
    'walking',
    'collecting_resource'
);

CREATE TABLE character.instance(
       name TEXT NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       username TEXT NOT NULL,
       FOREIGN KEY (e_mail, username) REFERENCES player.record(e_mail, username),
       PRIMARY KEY(name, username, e_mail)
);

CREATE TABLE character.stats(
       name TEXT NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
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
       FOREIGN KEY (name, username, e_mail) REFERENCES character.instance(name, username, e_mail),
       PRIMARY KEY(name, username, e_mail)
);

CREATE TABLE character.position(
       name TEXT NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
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
       FOREIGN KEY (name, username, e_mail) REFERENCES character.instance(name, username, e_mail),
       FOREIGN KEY (map_name) REFERENCES map.instance(name),
       PRIMARY KEY(name, username, e_mail)
);

CREATE TABLE character.active(
       name TEXT NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       username TEXT NOT NULL,
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

    INSERT INTO character.stats(name, e_mail, username, constitution, wisdom, strength, endurance, intelligence, faith, level, health_max, health, mana, mana_max)
    VALUES (NEW.name, NEW.e_mail, NEW.username, NEW.constitution, NEW.wisdom, NEW.strength, NEW.endurance, NEW.intelligence, NEW.faith, NEW.level, NEW.health_max, NEW.health, NEW.mana, NEW.mana_max)
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
       level = NEW.level,
       health_max = NEW.health_max,
       health = NEW.health,
       mana_max = NEW.mana_max,
       mana = NEW.mana;

    INSERT INTO character.position(name, e_mail, username, x_position, y_position, x_velocity, y_velocity, map_name, face_direction, state_type)
    VALUES (NEW.name, NEW.e_mail, NEW.username, NEW.x_position, NEW.y_position, NEW.x_velocity, NEW.y_velocity, NEW.map_name, NEW.face_direction, NEW.state_type)
    ON CONFLICT (name, username, e_mail) DO UPDATE SET
           name = NEW.name,
           e_mail = NEW.e_mail,
           username = NEW.username,
           x_position = NEW.x_position,
           y_position = NEW.y_position,
           x_velocity = NEW.x_velocity,
           y_velocity = NEW.y_velocity,	   
           state_type = NEW.state_type,
	   face_direction = NEW.face_direction,
           map_name = NEW.map_name;

    RETURN NEW;
END
$$;

CREATE TABLE character.item(
       name TEXT NOT NULL,
       description TEXT NOT NULL,
       weight SMALLINT NOT NULL,
       PRIMARY KEY(name)
);

CREATE TABLE character.inventory(
       name TEXT NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       username TEXT NOT NULL,
       quantity SMALLINT NOT NULL,
       item_name TEXT NOT NULL,
       FOREIGN KEY (name, username, e_mail) REFERENCES character.instance(name, username, e_mail),
       FOREIGN KEY (item_name) REFERENCES character.item(name),       
       PRIMARY KEY (name, username, e_mail, item_name)
);

CREATE OR REPLACE PROCEDURE character.update_inventory
   (new_name TEXT, new_e_mail TEXT, new_username TEXT, new_item_name TEXT, new_quantity SMALLINT)
   LANGUAGE sql AS
$$
  INSERT INTO character.inventory(name, e_mail, username, quantity, item_name)
  VALUES (new_name, new_e_mail, new_username, new_quantity, new_item_name)
  ON CONFLICT (name, e_mail, username, item_name)
  DO UPDATE SET
    -- EXCLUDED is an alias to the row that is conflicting
    -- https://www.postgresql.org/docs/17/sql-insert.html
    -- so this line is basically the sum of old + new
    quantity = EXCLUDED.quantity + character.inventory.quantity;
$$;

CREATE OR REPLACE TRIGGER trigger_character_upsert
INSTEAD OF INSERT ON character.view
FOR EACH ROW EXECUTE FUNCTION character.view_upsert();

CREATE OR REPLACE TRIGGER trigger_character_upsert
INSTEAD OF UPDATE ON character.view
FOR EACH ROW EXECUTE FUNCTION character.view_upsert();
