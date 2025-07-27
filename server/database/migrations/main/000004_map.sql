-- TYPES
DO $$ BEGIN
    -- TODO: Deal with this separately on #96, aparently this causes a weird
    --       BUG on epgsql, need to improve the testing suite until I can 
    --       investigate this properly.
    -- Map Tile
    -- IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'map.TILE_TYPE') THEN
    --   CREATE DOMAIN map.TILE_TYPE AS TEXT 
    --   CONSTRAINT CHECK_TILE_TYPE
    --   NOT NULL CHECK (VALUE IN (
    --     'EMPTY',
    --     'WATER',
    --     'GRASS',
    --     'SAND',
    --     'DIRT'
    --   ));
    -- END IF;
    --
    -- -- Object Type
    -- IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'map.OBJECT_TYPE') THEN
    --   CREATE DOMAIN map.OBJECT_TYPE AS TEXT 
    --   CONSTRAINT CHECK_TILE_TYPE
    --   NOT NULL CHECK (VALUE IN (
    --     'EMPTY',
    --     'BUSH',
    --     'CHEST',
    --     'ROCK', 
    --     'TREE'
    --   ));
    -- END IF;

    IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'map.OBJECT_TYPE') THEN
        CREATE TYPE map.TILE_TYPE AS ENUM(
            'EMPTY',
            'WATER',
            'GRASS',
            'SAND',
            'DIRT'
        );
    END IF;

    IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'map.OBJECT_TYPE') THEN
        CREATE TYPE map.OBJECT_TYPE AS ENUM(
            'EMPTY',
            'BUSH',
            'TREE',
            'CHEST',
            'ROCK'
        );
    END IF;

    -- Map Tile Input
    IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'map.tile_input') THEN
        CREATE TYPE map.tile_input AS (
            map_name TEXT,
            tile_type map.TILE_TYPE,
            x_position REAL,
            y_position REAL
        );
    END IF;
END $$;

-- https://yrashk.com/blog/2023/04/09/sum-types-in-postgres/
-- https://docs.omnigres.org/omni_types/sum_types/
-- Creates a SUM Type for map entities, to check it use 
--      table omni_types.sum_types;
-- ENTITY_TYPE = OBJECT_TYPE | TILE_TYPE
SELECT omni_types.sum_type('map.ENTITY_TYPE', 'map.OBJECT_TYPE', 'map.TILE_TYPE');

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

