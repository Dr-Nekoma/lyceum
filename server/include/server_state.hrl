-type email_pid() :: {Email :: string(), Pid :: pid()}.

-record(server_state,
        {connection :: epgsql:connection(), pid :: pid(), table :: [email_pid()] | ets:tab()}).

-type server_state() ::
    #server_state{connection :: epgsql:connection(),
                  pid :: pid(),
                  table :: [email_pid()]}.
