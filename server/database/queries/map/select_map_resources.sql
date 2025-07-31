SELECT 
    x_position, 
    y_position, 
    quantity, 
    kind, 
    capacity, 
    base_extraction_amount, 
    base_extraction_time, 
    item_pk 
FROM map.resource_view
WHERE map_name = $1::TEXT
