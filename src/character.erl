-module(character).

-export([create/2]).

create(#{name := Name, 
	 username := Username, 
	 email := Email,
	 constitution := Constitution,
	 wisdom := Wisdom,
	 strength := Strength,
	 endurance := Endurance,
	 intelligence := Intelligence,
	 faith := Faith}, Connection) ->
    Query = "INSERT INTO lyceum.\"view_character\" (\"name\",  \"e-mail\", \"username\", \"constitution\", \"wisdom\", \"strength\", \"endurance\", \"intelligence\", \"faith\") \
             VALUES ($1::VARCHAR(18), $2::TEXT, $3::VARCHAR(32), $4::SMALLINT, $5::SMALLINT, $6::SMALLINT, $7::SMALLINT, $8::SMALLINT, $9::SMALLINT)",
    {ok, _, _} = epgsql:equery(Connection, Query, [Name, Username, Email, Constitution, Wisdom, Strength, Endurance, Intelligence, Faith]).


%% INSERT INTO lyceum."view_character"("name", "e-mail", "username", "constitution", "wisdom", "strength", "endurance", "intelligence", "faith")
%% VALUES ('knight', 'test@email.com', 'test', 10, 12, 13, 14, 15, 16);


