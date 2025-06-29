CREATE IF NOT EXISTS EXTENSION citext;

CREATE DOMAIN player.email AS citext
  CHECK ( value ~ '^[a-zA-Z0-9.!#$%&''*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$' );

CREATE TABLE IF NOT EXISTS player.record(
    username TEXT NOT NULL,
    -- TODO: for debug purposes only, do this properly later
    password TEXT NOT NULL,
    e_mail player.email NOT NULL,
    PRIMARY KEY(username, e_mail)
);
