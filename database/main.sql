CREATE TABLE "user"(
       "username" VARCHAR(32) NOT NULL,
       "password" TEXT NOT NULL,
       "e-mail" TEXT NOT NULL CHECK ("e-mail" ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       PRIMARY KEY("username", "e-mail")
);

select * from "user";

CREATE TABLE "map"(
       "name" VARCHAR(16) NOT NULL,
       PRIMARY KEY("name")
);

CREATE TYPE TILE_TYPE AS ENUM(
       'WATER',
       'GRASS',
       'SAND',
       'ROCK'
);

CREATE TABLE "tile"(
       "map_name" VARCHAR(16) NOT NULL, 
       "kind" TILE_TYPE NOT NULL,
       "x_position" SMALLINT NOT NULL,
       "y_position" SMALLINT NOT NULL,
       PRIMARY KEY("map_name", "kind", "x_position", "y_position"),
       FOREIGN KEY ("map_name") REFERENCES "map"("name")
);

CREATE TABLE "object"(
       "map_name" VARCHAR(16) NOT NULL, 
       "name" VARCHAR(16) NOT NULL,
       "x_position" SMALLINT NOT NULL,
       "y_position" SMALLINT NOT NULL,
       PRIMARY KEY("map_name", "x_position", "y_position"),
       FOREIGN KEY ("map_name") REFERENCES "map"("name")
);

CREATE OR REPLACE FUNCTION map_object_overlap() RETURNS trigger AS $map_object_overlap$
       DECLARE
	garbage FOR SELECT "kind" FROM "tile" WHERE "x_position" = NEW."x_position" AND "y_position" = NEW."y_position"
       BEGIN
	IF NEW."name" = 'TREE' THEN
	   CASE WHEN garbage = 'WATER' OR garbage = 'ROCK' THEN RAISE EXCEPTION '''TREE'' cannot be defined in tiles that are not ''GRASS'' or ''SAND''.';
	   ELSE RETURN NEW;
	   END;
	END IF;
	RETURN NEW;
       END;
$map_object_overlap$ LANGUAGE plpgsql;

CREATE TRIGGER map_object_overlap BEFORE INSERT OR UPDATE ON "object"
FOR EACH ROW EXECUTE FUNCTION map_object_overlap();

CREATE TABLE "character"(
       "name" VARCHAR(18) NOT NULL,
       "e-mail" TEXT NOT NULL,
       "username" VARCHAR(32) NOT NULL,
       FOREIGN KEY ("e-mail", "username") REFERENCES "user"("e-mail", "username"),
       PRIMARY KEY("name", "username", "e-mail")
);

CREATE TABLE "character_stats"(
       "name" VARCHAR(18) NOT NULL,
       "username" VARCHAR(32) NOT NULL,
       "constitution" SMALLINT NOT NULL CHECK ("constitution" > 0 AND "constitution" <= 150),
       "wisdom" SMALLINT NOT NULL CHECK ("wisdom" > 0 AND "wisdom" <= 150),
       "strength" SMALLINT NOT NULL CHECK ("strength" > 0 AND "strength" <= 150),
       "endurance" SMALLINT NOT NULL CHECK ("endurance" > 0 AND "endurance" <= 150),
       "intelligence" SMALLINT NOT NULL CHECK ("intelligence" > 0 AND "intelligence" <= 150),
       "faith" SMALLINT NOT NULL CHECK ("faith" > 0 AND "faith" <= 150),
       FOREIGN KEY ("name", "username", "e-mail") REFERENCES "character"("name", "username", "e-mail"),
       PRIMARY KEY("name", "username")
);

CREATE TABLE "character_position"(
       "name" VARCHAR(18) NOT NULL,
       "username" VARCHAR(32) NOT NULL,
       "x_position" SMALLINT NOT NULL,
       "y_position" SMALLINT NOT NULL,
       "map_name" VARCHAR(64) NOT NULL,
       FOREIGN KEY ("name", "username") REFERENCES "character"("name", "username"),
       FOREIGN KEY ("map_name") REFERENCES "map"("name"),
       PRIMARY KEY("name", "username", "map_name")
);

