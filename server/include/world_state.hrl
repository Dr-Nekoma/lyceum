-record(map_coordinates, {x :: non_neg_integer(), y :: non_neg_integer()}).
-record(map_dimension, {width :: non_neg_integer(), height :: non_neg_integer()}).
-record(map_properties, {name :: nonempty_string(), dimension :: map_dimension()}).
-record(world_state, {map :: map_properties(), connection :: epgsql:connection()}).

-type map_dimension() ::
    #map_dimension{width :: non_neg_integer(), height :: non_neg_integer()}.
-type map_properties() ::
    #map_properties{name :: nonempty_string(), dimension :: map_dimension()}.
-type world_state() ::
    #world_state{map :: map_properties(), connection :: epgsql:connection()}.
