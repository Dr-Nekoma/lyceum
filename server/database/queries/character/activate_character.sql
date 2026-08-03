-- Opens a presence period, unless one is already open.
--
-- The old form was INSERT ... ON CONFLICT DO NOTHING, which relied on
-- the row-per-character key to make activating twice harmless. Now that
-- a character may have many periods, "already active" is a question
-- about the open one, so ask it directly. The temporal primary key is
-- still the backstop if two nodes ask at once.
INSERT INTO character.active (name, email, username)
SELECT $1::TEXT, $2::TEXT, $3::TEXT
WHERE NOT EXISTS (
    SELECT 1 FROM character.active
    WHERE name = $1::TEXT
      AND email = $2::TEXT
      AND username = $3::TEXT
      AND upper(valid_at) = 'infinity'
)
