INSERT INTO character.view (name, email, username, constitution, wisdom, strength, endurance, intelligence, faith)
VALUES ($1::TEXT, $2::TEXT, $3::TEXT, $4::SMALLINT, $5::SMALLINT, $6::SMALLINT, $7::SMALLINT, $8::SMALLINT, $9::SMALLINT)"
