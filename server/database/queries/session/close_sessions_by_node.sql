UPDATE player.session
SET valid_at = tstzrange(lower(valid_at), clock_timestamp(), '[)')
WHERE owner_node = $1::TEXT AND upper(valid_at) = 'infinity'
