UPDATE character.view
SET x_position = $1::REAL,
    y_position = $2::REAL,
    x_velocity = $3::REAL,
    y_velocity = $4::REAL,
    level = $5::SMALLINT,
    health = $6::SMALLINT,
    mana = $7::SMALLINT,
    face_direction = $8::SMALLINT,
    state_type = $9::\"character\".STATE_TYPE
WHERE 
    name = $10::TEXT
AND email = $11::TEXT
AND username = $12::TEXT 
AND map_name = $13::TEXT
