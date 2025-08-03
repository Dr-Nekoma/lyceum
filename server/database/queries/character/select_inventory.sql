SELECT 
    item_name,
    quantity
FROM map.resource_item_view
WHERE
    name = $1::TEXT
AND username = $2::TEXT
AND email = $3::TEXT
LIMIT 1
