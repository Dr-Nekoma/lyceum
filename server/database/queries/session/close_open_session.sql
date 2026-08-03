UPDATE player.session
SET valid_at = tstzrange(lower(valid_at), clock_timestamp(), '[)')
WHERE player_id = $1::BIGINT AND upper(valid_at) = 'infinity'
