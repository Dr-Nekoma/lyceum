-- Closes the open presence period rather than deleting the row, so how
-- long the character was in the world survives them leaving it.
UPDATE character.active
SET valid_at = tstzrange(lower(valid_at), clock_timestamp(), '[)')
WHERE
    name = $1::TEXT
AND email = $2::TEXT
AND username = $3::TEXT
AND upper(valid_at) = 'infinity'
