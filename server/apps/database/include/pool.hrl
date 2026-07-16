-type pool_name() :: lyceum_pool | auth_pool | mnesia_pool.
-type pool_role() :: application | auth | mnesia.
-type pool_config() :: #{name := pool_name(), role := pool_role(), size := pos_integer()}.
-type query_result() ::
    #{command := atom(), num_rows := non_neg_integer(), rows := [tuple() | map()]}
    | {error, term()}.
