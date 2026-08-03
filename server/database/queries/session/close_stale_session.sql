UPDATE player.session
SET valid_at = tstzrange(lower(valid_at), clock_timestamp(), '[)')
WHERE player_id = $1::BIGINT
  AND owner_node = $2::TEXT
  AND upper(valid_at) = 'infinity'
  AND lower(valid_at) < clock_timestamp() - ($3::INTEGER * INTERVAL '1 second')
