INSERT INTO player.session
    (player_id, username, email, client_pid, client_node, owner_node)
VALUES
    ($1::BIGINT, $2::TEXT, $3::player.email, $4::BYTEA, $5::TEXT, $6::TEXT)
