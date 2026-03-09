-- These indexes are actually an WIP and I need some better profiling before 
-- making them official. This is just here to make the demo a little bit
-- better.

-- ----------------------------------------------------------------------------
-- QUERY: UPDATE character.view
-- ----------------------------------------------------------------------------
-- Both of these are an attempt to make index-only scans when using the view
-- for reading and inserting (respectively).
CREATE INDEX IF NOT EXISTS idx_character_position_lookup
ON character.position (name, email, username, map_name)
INCLUDE (
    x_position, y_position, x_velocity, y_velocity, face_direction, state_type
);

CREATE INDEX IF NOT EXISTS idx_character_stats_lookup
ON character.stats (name, email, username)
INCLUDE (
    level,
    health,
    mana,
    constitution,
    wisdom,
    strength,
    endurance,
    intelligence,
    faith,
    health_max,
    mana_max
);

-- ----------------------------------------------------------------------------
-- QUERY: UPDATE character.stats
-- ----------------------------------------------------------------------------
-- The primary key (name, email, username) is already indexed.
-- But we can optimize the foreign key check with a better index order
CREATE INDEX IF NOT EXISTS idx_character_instance_reverse
ON character.instance (name, username, email);


-- ----------------------------------------------------------------------------
-- QUERY: SELECT FROM character.view JOIN character.active
-- ----------------------------------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_character_position_map_lookup
ON character.position (map_name, name)
INCLUDE (
    email, username, x_position, y_position, x_velocity, y_velocity,
    face_direction, state_type
);

CREATE INDEX IF NOT EXISTS idx_character_stats_join
ON character.stats (name, username, email)
INCLUDE (
    constitution, wisdom, strength, endurance, intelligence, faith,
    level, health_max, health, mana_max, mana
);
