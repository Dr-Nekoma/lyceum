CREATE SCHEMA lyceum;

CREATE TABLE lyceum.user(
       username VARCHAR(32) NOT NULL,
       password TEXT NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       PRIMARY KEY(username, e_mail)
);

CREATE TABLE lyceum.map(
       name VARCHAR(16) NOT NULL,
       PRIMARY KEY(name)
);

CREATE TABLE lyceum.character(
       name VARCHAR(18) NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       username VARCHAR(32) NOT NULL,
       FOREIGN KEY (e_mail, username) REFERENCES lyceum.user(e_mail, username),
       PRIMARY KEY(name, username, e_mail)
);

CREATE TABLE lyceum.character_stats(
       name VARCHAR(18) NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       username VARCHAR(32) NOT NULL,
       constitution SMALLINT NOT NULL CHECK (constitution > 0 AND constitution <= 150),
       wisdom SMALLINT NOT NULL CHECK (wisdom > 0 AND wisdom <= 150),
       strength SMALLINT NOT NULL CHECK (strength > 0 AND strength <= 150),
       endurance SMALLINT NOT NULL CHECK (endurance > 0 AND endurance <= 150),
       intelligence SMALLINT NOT NULL CHECK (intelligence > 0 AND intelligence <= 150),
       faith SMALLINT NOT NULL CHECK (faith > 0 AND faith <= 150),
       FOREIGN KEY (name, username, e_mail) REFERENCES lyceum.character(name, username, e_mail),
       PRIMARY KEY(name, username, e_mail)
);

CREATE TABLE lyceum.character_position(
       name VARCHAR(18) NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       username VARCHAR(32) NOT NULL,
       x_position DECIMAL(4, 2) NOT NULL,
       y_position SMALLINT NOT NULL,
       map_name VARCHAR(64) NOT NULL,
       FOREIGN KEY (name, username, e_mail) REFERENCES lyceum.character(name, username, e_mail),
       FOREIGN KEY (map_name) REFERENCES lyceum.map(name),
       PRIMARY KEY(name, username, e_mail, map_name)
);

CREATE VIEW lyceum.view_character AS
EXPLAIN
SELECT * FROM lyceum.character
NATURAL JOIN lyceum.character_stats
NATURAL JOIN lyceum.character_position;

CREATE OR REPLACE FUNCTION lyceum.view_character_insert() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
    INSERT INTO lyceum.character(name, e_mail, username)
    VALUES (NEW.name, NEW.e_mail, NEW.username);

    INSERT INTO lyceum.character_stats(name, e_mail, username, constitution, wisdom, strength, endurance, intelligence, faith)
    VALUES (NEW.name, NEW.e_mail, NEW.username, NEW.constitution, NEW.wisdom, NEW.strength, NEW.endurance, NEW.intelligence, NEW.faith);

    INSERT INTO lyceum.character_position(name, e_mail, username, x_position, y_position, map_name)
    VALUES (NEW.name, NEW.e_mail, NEW.username, NEW.x_position, NEW.y_position, NEW.map_name);

    RETURN NEW;
END
$$;

CREATE TRIGGER trigger_character_insert
INSTEAD OF INSERT ON lyceum.view_character
FOR EACH ROW EXECUTE FUNCTION lyceum.view_character_insert();

-- INSERT INTO lyceum.user(username, e_mail, password)
-- VALUES ('test', 'test@email.com', '123');

-- INSERT INTO lyceum.view_character(name, e_mail, username, constitution, wisdom, strength, endurance, intelligence, faith)
-- VALUES ('knight', 'test@email.com', 'test', 10, 12, 13, 14, 15, 16);
