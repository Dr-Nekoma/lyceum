-module(resource).

-export([update/2]).
-compile({parse_transform, do}).

update(#{map_name := MapName, 
	 kind := Kind, 
	 x_position := XPosition,
	 y_position := YPosition,
	 quantity := Quantity},
       Connection) ->
    Query = "UPDATE map.resource_view \
             SET quantity = $1::SMALLINT \
             WHERE map_name = $2::TEXT and kind = $3::\"map\".OBJECT_TYPE and x_position = $4::REAL and y_position = $5::REAL",
    Result = epgsql:with_transaction(Connection, 
				      fun (Conn) -> epgsql:equery(Conn, Query, [Quantity, MapName, Kind, XPosition, YPosition])
				      end,
				      #{ begin_opts => "ISOLATION LEVEL READ UNCOMMITTED"}),
    do([postgres_m || 
	   _ <- {Result, update},
	   epgsql:sync(Connection)]).

