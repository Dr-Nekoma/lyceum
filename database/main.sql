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
       "x_position" DECIMAL(4, 2) NOT NULL,
       "y_position" SMALLINT NOT NULL,
       "map_name" VARCHAR(64) NOT NULL,
       FOREIGN KEY ("name", "username") REFERENCES "character"("name", "username"),
       FOREIGN KEY ("map_name") REFERENCES "map"("name"),
       PRIMARY KEY("name", "username", "map_name")
);

