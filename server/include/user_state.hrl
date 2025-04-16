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
