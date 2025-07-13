-record(dispatcher_state, {connection :: epgsql:connection(), pid :: pid()}).

-type dispatcher_state() ::
    #dispatcher_state{connection :: epgsql:connection(), pid :: pid()}.
