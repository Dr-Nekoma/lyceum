SELECT
    character.view.name,
    character.view.constitution,
    character.view.wisdom,
    character.view.strength,
    character.view.endurance,
    character.view.intelligence,
    character.view.faith,
    character.view.x_position,
    character.view.y_position,
    character.view.x_velocity,
    character.view.y_velocity,
    character.view.map_name,
    character.view.face_direction,
    character.view.level,
    character.view.health_max,
    character.view.health,
    character.view.mana_max,
    character.view.mana,
    character.view.state_type
FROM character.view
NATURAL JOIN character.active
WHERE
    map_name = $1::TEXT
    AND name <> $2::TEXT
