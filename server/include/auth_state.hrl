-record(auth_state, {connection :: epgsql:connection(), pid :: pid()}).

-type auth_state() ::
    #auth_state{connection :: epgsql:connection(), pid :: pid()}.
