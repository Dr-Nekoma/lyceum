-- Insert into lyceum.user
INSERT INTO lyceum.user (username, password, e_mail)
VALUES ('mmagueta', 'password123', 'mmagueta@example.com');

-- Insert into lyceum.map
INSERT INTO lyceum.map (name)
VALUES ('CASTLE_HALL'), ('CASTLE_YARD');

-- Insert into lyceum.character and related tables
INSERT INTO lyceum.character (name, e_mail, username)
VALUES ('Huneric', 'mmagueta@example.com', 'mmagueta'),
       ('Gaiseric', 'mmagueta@example.com', 'mmagueta');

INSERT INTO lyceum.character_stats (name, e_mail, username, constitution, wisdom, strength, endurance, intelligence, faith)
VALUES ('Huneric', 'mmagueta@example.com', 'mmagueta', 100, 110, 95, 120, 105, 100),
       ('Gaiseric', 'mmagueta@example.com', 'mmagueta', 60, 110, 55, 150, 150, 50);

INSERT INTO lyceum.character_position (name, e_mail, username, x_position, y_position, map_name)
VALUES ('Huneric', 'mmagueta@example.com', 'mmagueta', 10, 20, 'CASTLE_HALL'),
       ('Gaiseric', 'mmagueta@example.com', 'mmagueta', 15, 15, 'CASTLE_HALL');

-- Insert into lyceum.equipment
INSERT INTO lyceum.equipment (name, description, kind)
VALUES
('Vandal''s Prima', 'A rugged sword of unknown origin, favored by the wild tribes of the West. Its hilt is adorned with a crude carving depicting a dragon''s defeat by a golden warrior. It is said to be a symbol of the tribes'' turbulent beginnings.', 'ARMS'),
('Blood-Reaver''s Gauntlet', 'Fashioned from crude iron in the crucible of barbarian rites, this gauntlet bears primal insignias and the echoes of ancient battles. It is said that those who don this gauntlet unleash the untamed fury of ancestral warriors, their strikes echoing the relentless tide of bloodshed from when the barbarians were feared by the Joniens.', 'ARMS');

-- Insert into lyceum.character_equipment
INSERT INTO lyceum.character_equipment (name, e_mail, username, is_equiped, equipment_name, use, kind)
VALUES ('Huneric', 'mmagueta@example.com', 'mmagueta', true, 'Vandal''s Prima', 'RIGHT_ARM', 'ARMS'::lyceum.equipment_kind),
       ('Huneric', 'mmagueta@example.com', 'mmagueta', true, 'Blood-Reaver''s Gauntlet', 'ARMS'::lyceum.equipment_use, 'ARMS'::lyceum.equipment_kind),
       ('Gaiseric', 'mmagueta@example.com', 'mmagueta', true, 'Vandal''s Prima', 'RIGHT_ARM', 'ARMS'::lyceum.equipment_kind);
