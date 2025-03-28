-- Insert into lyceum.user
INSERT INTO player.record (username, password, e_mail)
VALUES ('mmagueta', 'password123', 'mmagueta@example.com'),
       ('benin', 'sekrit', 'benin@example.com'),
       ('marinho', 'scheme', 'marinho@example.com'),
       ('nathan', 'rescript', 'nathan@example.com'),
       ('lemos', 'pass123', 'lemos@example.com');

-- Insert into lyceum.map
INSERT INTO map.instance (name, width, height)
VALUES ('Pond', 6, 6);

INSERT INTO character.item (name, description, weight)
VALUES ('Dolomite', 'In this world, dolomite is used as a valuable currency by the empires of the East.', 42),
       ('Great Bardook Branch', 'A tree that has been planted by the goddess Caithee.', 5),
       ('Sussman''s Lispy Fez', 'The magical headgear from a once great wizard.', 1);

-- Should insert into map.resource_view instead, but that's currently broken.
INSERT INTO map.object_is_resource (kind, capacity, base_extraction_amount, base_extraction_time, name, description, item_pk)
VALUES ('ROCK'::map.OBJECT_TYPE, 50, 10, 1600, 'Fluorite', 'It is blue and pretty.', 'Dolomite'),
       ('TREE'::map.OBJECT_TYPE, 50, 1, 1600, 'Willow', 'Likes water.', 'Great Bardook Branch');

-- Insert into lyceum.character and related tables
INSERT INTO character.instance (name, e_mail, username)
VALUES ('Huneric', 'mmagueta@example.com', 'mmagueta'),
       ('Gaiseric', 'mmagueta@example.com', 'mmagueta'),
       ('Scipio', 'benin@example.com', 'benin'),
       ('Silver', 'marinho@example.com', 'marinho'),
       ('Camler', 'nathan@example.com', 'nathan'),
       ('Legion', 'lemos@example.com', 'lemos');

INSERT INTO character.stats (name, e_mail, username, constitution, wisdom, strength, endurance, intelligence, faith, level, health, mana)
VALUES ('Huneric', 'mmagueta@example.com', 'mmagueta', 100, 110, 95, 120, 105, 100, 17, 75, 50),
       ('Gaiseric', 'mmagueta@example.com', 'mmagueta', 60, 110, 55, 150, 150, 50, 22, 67, 33),
       ('Scipio', 'benin@example.com', 'benin', 90, 110, 75, 120, 150, 50, 22, 67, 33),
       ('Silver', 'marinho@example.com', 'marinho', 90, 110, 70, 90, 145, 90, 22, 67, 33),
       ('Camler', 'nathan@example.com', 'nathan', 90, 110, 75, 120, 150, 50, 22, 67, 33),
       ('Legion', 'lemos@example.com', 'lemos', 60, 110, 55, 150, 150, 50, 15, 85, 77);

INSERT INTO character.position (name, e_mail, username, x_position, y_position, map_name, face_direction)
VALUES ('Huneric', 'mmagueta@example.com', 'mmagueta', 10, 10, 'Pond', 270),
       ('Gaiseric', 'mmagueta@example.com', 'mmagueta', 10, 10, 'Pond', 270),
       ('Scipio', 'benin@example.com', 'benin', 10, 10, 'Pond', 270),
       ('Silver', 'marinho@example.com', 'marinho', 10, 10, 'Pond', 270),
       ('Camler', 'nathan@example.com', 'nathan', 10, 10, 'Pond', 270),
       ('Legion', 'lemos@example.com', 'lemos', 10, 10, 'Pond', 270);

-- Insert into lyceum.equipment
INSERT INTO equipment.instance (name, description, kind)
VALUES
('Vandal''s Prima', 'A rugged sword of unknown origin, favored by the wild tribes of the West. Its hilt is adorned with a crude carving depicting a dragon''s defeat by a golden warrior. It is said to be a symbol of the tribes'' turbulent beginnings.', 'ARMS'),
('Legate''s Pugio', 'A dagger commonly used by the legionaries, produced in large numbers to obey the strict standards of the res militaris.', 'ARMS'),
('Blood-Reaver''s Gauntlet', 'Fashioned from crude iron in the crucible of barbarian rites, this gauntlet bears primal insignias and the echoes of ancient battles. It is said that those who don this gauntlet unleash the untamed fury of ancestral warriors, their strikes echoing the relentless tide of bloodshed from when the barbarians were feared by the Joniens.', 'ARMS');

-- Insert into lyceum.character_equipment
INSERT INTO equipment.character (name, e_mail, username, is_equiped, equipment_name, use, kind)
VALUES ('Huneric', 'mmagueta@example.com', 'mmagueta', true, 'Vandal''s Prima', 'RIGHT_ARM', 'ARMS'::equipment.kind),
       ('Huneric', 'mmagueta@example.com', 'mmagueta', true, 'Blood-Reaver''s Gauntlet', 'ARMS'::equipment.use, 'ARMS'::equipment.kind),
       ('Gaiseric', 'mmagueta@example.com', 'mmagueta', true, 'Vandal''s Prima', 'RIGHT_ARM', 'ARMS'::equipment.kind),
       ('Scipio', 'benin@example.com', 'benin', true, 'Legate''s Pugio', 'RIGHT_ARM', 'ARMS'::equipment.kind),
       ('Legion', 'lemos@example.com', 'lemos', true, 'Vandal''s Prima', 'RIGHT_ARM', 'ARMS'::equipment.kind);
