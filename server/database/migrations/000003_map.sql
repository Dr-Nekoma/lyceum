-- TYPES
DO $$ BEGIN
    -- TILE_TYPE
    IF to_regtype('map.TILE_TYPE') IS NULL THEN
        CREATE TYPE map.TILE_TYPE AS ENUM(
            'EMPTY',
            'WATER',
            'GRASS',
            'SAND',
            'DIRT'
        );
    END IF;

    -- OBJECT_TYPE
    IF to_regtype('map.OBJECT_TYPE') IS NULL THEN
        CREATE TYPE map.OBJECT_TYPE AS ENUM(
            'EMPTY',
            'BUSH',
            'TREE',
            'CHEST',
            'ROCK'
        );
    END IF;

    -- These records are used as function arguments
    IF to_regtype('map.input') IS NULL THEN
        CREATE TYPE map.input AS (
            map_name TEXT,
            kind map.TILE_TYPE,
            x_position REAL,
            y_position REAL
        );
    END IF;
END $$;

-- TABLES
CREATE TABLE IF NOT EXISTS map.instance(
       name TEXT NOT NULL,
       width INTEGER NOT NULL,
       height INTEGER NOT NULL,
       PRIMARY KEY(name)
);

CREATE TABLE IF NOT EXISTS map.tile(
       map_name TEXT NOT NULL, 
       kind map.TILE_TYPE NOT NULL,
       x_position REAL NOT NULL,
       y_position REAL NOT NULL,
       PRIMARY KEY(map_name, kind, x_position, y_position),
       FOREIGN KEY (map_name) REFERENCES map.instance(name)
);

CREATE TABLE IF NOT EXISTS map.object(
       map_name TEXT NOT NULL,
       -- TODO: Add constraint depending on the same kind and position of the tile.
       -- Some objects should only be able to put on top of if they are on a specific kind of tile
       kind map.OBJECT_TYPE NOT NULL,
       x_position REAL NOT NULL,
       y_position REAL NOT NULL,
       -- TODO: Some items have face direction for us to care, e.g., like chest
       -- face_direction SMALLINT NOT NULL CHECK (face_direction >= 0 AND face_direction < 360) DEFAULT 270,		
       PRIMARY KEY(map_name, kind, x_position, y_position),
       FOREIGN KEY (map_name) REFERENCES map.instance(name)
);

-- Functions & Triggers
-- https://stackoverflow.com/a/39605068/4614840
CREATE OR REPLACE FUNCTION map.upsert(_tbl regclass, map.input[], OUT result INTEGER)
  LANGUAGE plpgsql AS
$$
BEGIN
   EXECUTE format('
    INSERT INTO %s(map_name, kind, x_position, y_position)
    SELECT map_name, kind, x_position, y_position
    FROM unnest($1)
    ON CONFLICT (map_name, kind, x_position, y_position)
    DO UPDATE
        SET 
            kind = kind,
            x_position = x_position,
            y_position = y_position;
   ', _tbl)
   INTO result;
END
$$;

-- CREATE OR REPLACE FUNCTION map_object_overlap() RETURNS trigger AS $map_object_overlap$
-- DECLARE 
--     kind TEXT;
-- BEGIN
--     SELECT kind INTO kind FROM tile WHERE x_position = NEW.x_position AND y_position = NEW.y_position;

--     IF NEW.name = 'TREE' THEN
--         IF kind <> 'GRASS' AND kind <> 'SAND' THEN
--             RAISE EXCEPTION '''TREE'' cannot be defined in tiles that are not ''GRASS'' or ''SAND''.';
--         END IF;
--     END IF;

--     RETURN NEW;
-- END;
-- $map_object_overlap$ LANGUAGE plpgsql;

-- CREATE TRIGGER map_object_overlap BEFORE INSERT OR UPDATE ON map.object
-- FOR EACH ROW EXECUTE FUNCTION map_object_overlap();
