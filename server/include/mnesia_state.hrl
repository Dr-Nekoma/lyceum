-type mnesia_query_error() :: not_found | inconsistent_data | generic_error.

-record(mnesia_state, {}).

-type mnesia_state() :: #mnesia_state{}.
