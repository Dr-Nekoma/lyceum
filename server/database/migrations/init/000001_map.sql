-- Initialize Maps
INSERT INTO map.instance (name, width, height)
VALUES ('Pond', 6, 6)
ON CONFLICT (name)
DO UPDATE SET
    width = excluded.width,
    height = excluded.height;

-- Add Items
INSERT INTO character.item (name, description, weight)
VALUES (
    'Dolomite',
    'In this world, dolomite is used as a valuable currency by the empires of the East.',
    42
),
(
    'Great Bardook Branch',
    'A tree that has been planted by the goddess Caithee.',
    5
)
ON CONFLICT (name)
DO UPDATE SET
    description = excluded.description,
    weight = excluded.weight;

-- Add Resources
INSERT INTO map.object_is_resource (
    kind,
    capacity,
    base_extraction_amount,
    base_extraction_time,
    name,
    description,
    item_pk
)
VALUES (
    'ROCK'::map.OBJECT_TYPE,
    50,
    10,
    1600,
    'Fluorite',
    'It is blue and pretty.',
    'Dolomite'
),
(
    'TREE'::map.OBJECT_TYPE,
    50,
    1,
    1600,
    'Willow',
    'Likes water.',
    'Great Bardook Branch'
)
ON CONFLICT (kind)
DO NOTHING;
