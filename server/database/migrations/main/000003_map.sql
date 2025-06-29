-- TYPES
CREATE TYPE map.TILE_TYPE AS ENUM(
    'EMPTY',
    'WATER',
    'GRASS',
    'SAND',
    'DIRT'
);

CREATE TYPE map.OBJECT_TYPE AS ENUM(
    'EMPTY',
    'BUSH',
    'TREE',
    'CHEST',
    'ROCK'
);

CREATE TYPE map.input AS (
    map_name TEXT,
    kind map.TILE_TYPE,
    x_position REAL,
    y_position REAL
);

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

