-type mnesia_query_error() :: not_found | inconsistent_data | generic_error.

-record(mnesia_state, {connection :: epgsql:connection()}).

-type mnesia_state() :: #mnesia_state{connection :: epgsql:connection()}.
