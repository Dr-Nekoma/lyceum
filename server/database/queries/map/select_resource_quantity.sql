SELECT 
    quantity
FROM map.resource 
WHERE 
    map_name = $1::TEXT
AND x_position = $2::REAL 
AND y_position = $3::REAL 
AND kind = $4::"map".OBJECT_TYPE 
LIMIT 1
