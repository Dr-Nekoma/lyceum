SELECT client_pid, client_node, owner_node
FROM player.session
WHERE player_id = $1::BIGINT AND upper(valid_at) = 'infinity'
FOR UPDATE
