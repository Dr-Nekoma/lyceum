-- FUNCTIONS
-- https://stackoverflow.com/a/39605068/4614840
CREATE OR REPLACE FUNCTION map.upsert(_tbl regclass, map.tile_input[], OUT result INTEGER)
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

-- TRIGGERS
-- CREATE TRIGGER map_object_overlap BEFORE INSERT OR UPDATE ON map.object
-- FOR EACH ROW EXECUTE FUNCTION map_object_overlap();
