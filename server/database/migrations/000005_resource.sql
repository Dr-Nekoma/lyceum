CREATE TABLE IF NOT EXISTS map.object_is_resource(
       kind map.OBJECT_TYPE NOT NULL,
       capacity INTEGER NOT NULL CHECK (0 < capacity),
       base_extraction_amount INTEGER NOT NULL CHECK (0 < base_extraction_amount),
       base_extraction_time INTEGER NOT NULL CHECK (0 < base_extraction_time),
       name TEXT NOT NULL,
       description TEXT NOT NULL,
       item_pk TEXT NOT NULL,
       PRIMARY KEY (kind),
       FOREIGN KEY (item_pk) REFERENCES character.item(name)
);

CREATE TABLE IF NOT EXISTS map.resource(
       map_name TEXT NOT NULL,
       -- TODO: Add constraint depending on the same kind and position of the tile.
       -- Some objects should only be able to put on top of if they are on a specific kind of tile
       kind map.OBJECT_TYPE NOT NULL,
       quantity INTEGER NOT NULL DEFAULT 50 CHECK (0 <= quantity),
       x_position REAL NOT NULL,
       y_position REAL NOT NULL,
       -- TODO: Some items have face direction for us to care, e.g., like chest
       -- face_direction SMALLINT NOT NULL CHECK (face_direction >= 0 AND face_direction < 360) DEFAULT 270,
       PRIMARY KEY (map_name, kind, x_position, y_position),
       FOREIGN KEY (kind) REFERENCES map.object_is_resource(kind),
       FOREIGN KEY (map_name, kind, x_position, y_position) REFERENCES map.object(map_name, kind, x_position, y_position)
);

CREATE OR REPLACE VIEW map.resource_view AS
SELECT * FROM map.resource
NATURAL JOIN map.object_is_resource
NATURAL JOIN map.object;

CREATE OR REPLACE FUNCTION map.resource_management_view() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
    IF NEW.quantity = 0
     THEN
       DELETE FROM map.resource
       WHERE map_name = NEW.map_name and kind = NEW.kind and x_position = NEW.x_position and y_position = NEW.y_position;
     ELSE
       UPDATE map.resource
       SET quantity = NEW.quantity
       WHERE map_name = NEW.map_name and kind = NEW.kind and x_position = NEW.x_position and y_position = NEW.y_position;
     END IF;
END
$$;

CREATE TRIGGER resource_management_trigger
  INSTEAD OF INSERT OR UPDATE ON map.resource_view
  FOR EACH ROW
  EXECUTE PROCEDURE map.resource_management_view();

CREATE OR REPLACE FUNCTION object_is_resource() RETURNS TRIGGER
   LANGUAGE plpgsql AS
$$
  BEGIN
  IF EXISTS (SELECT kind, capacity FROM map.object_is_resource
              WHERE kind = new.kind AND new.quantity <= capacity)
   THEN
     RETURN new;
   END IF;
   RAISE exception 'Object is not a resource.';
END;
$$;

CREATE CONSTRAINT TRIGGER object_is_resource_trigger
  AFTER INSERT OR UPDATE ON map.resource
  DEFERRABLE INITIALLY IMMEDIATE
  FOR EACH ROW
  EXECUTE PROCEDURE object_is_resource();
