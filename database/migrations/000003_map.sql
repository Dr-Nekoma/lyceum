CREATE TABLE map.instance(
       name VARCHAR(16) NOT NULL,
       PRIMARY KEY(name)
);

CREATE TYPE map.TILE_TYPE AS ENUM(
       'WATER',
       'GRASS',
       'SAND',
       'ROCK'
);

CREATE TABLE map.tile(
       map_name VARCHAR(16) NOT NULL, 
       kind map.TILE_TYPE NOT NULL,
       x_position REAL NOT NULL,
       y_position REAL NOT NULL,
       PRIMARY KEY(map_name, kind, x_position, y_position),
       FOREIGN KEY (map_name) REFERENCES map.instance(name)
);

CREATE TABLE map.object(
       map_name VARCHAR(16) NOT NULL, 
       name VARCHAR(16) NOT NULL,
       x_position REAL NOT NULL,
       y_position REAL NOT NULL,
       PRIMARY KEY(map_name, x_position, y_position),
       FOREIGN KEY (map_name) REFERENCES map.instance(name)
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

CREATE TRIGGER map_object_overlap BEFORE INSERT OR UPDATE ON map.object
FOR EACH ROW EXECUTE FUNCTION map_object_overlap();
