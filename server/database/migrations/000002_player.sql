CREATE TABLE player.record(
       username VARCHAR(32) NOT NULL,
       password TEXT NOT NULL,
       e_mail TEXT NOT NULL CHECK (e_mail ~* '^[A-Za-z0-9.+%-]+@[A-Za-z0-9.-]+[.][A-Za-z]+$'),
       PRIMARY KEY(username, e_mail)
);
