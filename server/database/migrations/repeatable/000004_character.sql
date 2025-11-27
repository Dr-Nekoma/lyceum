-- VIEWS
CREATE OR REPLACE VIEW character.view AS
SELECT * FROM character.instance
NATURAL JOIN character.stats
NATURAL JOIN character.position;

-- FUNCTIONS
CREATE OR REPLACE FUNCTION character.view_upsert() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
    INSERT INTO character.instance(name, email, username)
    VALUES (NEW.name, NEW.email, NEW.username)
    ON CONFLICT DO NOTHING;

    INSERT INTO character.stats(name, email, username, constitution, wisdom, strength, endurance, intelligence, faith, level, health_max, health, mana, mana_max)
    VALUES (NEW.name, NEW.email, NEW.username, NEW.constitution, NEW.wisdom, NEW.strength, NEW.endurance, NEW.intelligence, NEW.faith, NEW.level, NEW.health_max, NEW.health, NEW.mana, NEW.mana_max)
    ON CONFLICT (name, email, username) DO UPDATE SET
       name = NEW.name,
       email = NEW.email,
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

    INSERT INTO character.position(name, email, username, x_position, y_position, x_velocity, y_velocity, map_name, face_direction, state_type)
    VALUES (NEW.name, NEW.email, NEW.username, NEW.x_position, NEW.y_position, NEW.x_velocity, NEW.y_velocity, NEW.map_name, NEW.face_direction, NEW.state_type)
    ON CONFLICT (name, username, email) DO UPDATE SET
           name = NEW.name,
           email = NEW.email,
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

-- PROCEDURES
CREATE OR REPLACE PROCEDURE character.update_inventory(
    new_name text,
    new_email text,
    new_username text,
    new_item_name text,
    new_quantity smallint
)
LANGUAGE sql AS
$$
  INSERT INTO character.inventory(name, email, username, quantity, item_name)
  VALUES (new_name, new_email, new_username, new_quantity, new_item_name)
  ON CONFLICT (name, email, username, item_name)
  DO UPDATE SET
    -- EXCLUDED is an alias to the row that is conflicting
    -- https://www.postgresql.org/docs/17/sql-insert.html
    -- so this line is basically the sum of old + new
    quantity = EXCLUDED.quantity + character.inventory.quantity;
$$;

-- TRIGGERS
CREATE OR REPLACE TRIGGER trigger_character_upsert
INSTEAD OF INSERT ON character.view
FOR EACH ROW EXECUTE FUNCTION character.view_upsert();

CREATE OR REPLACE TRIGGER trigger_character_upsert
INSTEAD OF UPDATE ON character.view
FOR EACH ROW EXECUTE FUNCTION character.view_upsert();
