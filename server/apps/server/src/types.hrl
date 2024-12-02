-type email_pid() :: {Email :: string(), Pid :: pid()}.

-record(server_state,
        {connection :: epgsql:connection(), pid :: pid(), table :: [email_pid()] | ets:tab()}).

-type server_state() ::
    #server_state{connection :: epgsql:connection(),
                  pid :: pid(),
                  table :: [email_pid()]}.

-record(user_state,
        {connection :: epgsql:connection(),
         pid :: pid(),
         email :: string(),
         username :: string(),
         name :: string()}).

-type user_state() ::
    #user_state{connection :: epgsql:connection(),
                pid :: pid(),
                email :: string(),
                username :: string(),
                name :: string()}.
