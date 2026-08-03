SELECT player_id, owner_node
FROM player.session
WHERE upper(valid_at) = 'infinity'
  AND lower(valid_at) < clock_timestamp() - ($1::INTEGER * INTERVAL '1 second')
ORDER BY lower(valid_at)
LIMIT $2::INTEGER
