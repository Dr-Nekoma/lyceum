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
-- Spelled out rather than NATURAL: character.active carries a validity
-- period now, and a natural join would happily match the closed ones
-- too, returning a player once per session they have ever had. Only the
-- open period means "in the world right now".
JOIN character.active
  ON character.active.name = character.view.name
 AND character.active.username = character.view.username
 AND character.active.email = character.view.email
 AND upper(character.active.valid_at) = 'infinity'
WHERE
    character.view.map_name = $1::TEXT
AND character.view.name <> $2::TEXT
