SELECT
    width,
    height
FROM map.instance
WHERE name = $1::TEXT
