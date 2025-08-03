SELECT 
    kind 
FROM map.object 
WHERE 
    map_name = $1::TEXT
ORDER BY y_position, x_position
