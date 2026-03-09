DELETE FROM character.active
WHERE
    name = $1::TEXT
    AND email = $2::TEXT
    AND username = $3::TEXT
