-type player_id() :: nonempty_string().
-type player_name() :: nonempty_string().
-type player_email() :: nonempty_string().

-record(player_data,
        {email :: player_email(), username :: player_name(), character_name :: player_name()}).

-type player_data() ::
    #player_data{email :: player_email(),
                 username :: player_name(),
                 character_name :: player_name()}.

-record(player_state,
        {player_id :: player_id(),
         client_pid :: pid(),
         connection :: epgsql:connection(),
         data :: player_data()}).

-type player_state() ::
    #player_state{player_id :: player_id(),
                  client_pid :: pid(),
                  connection :: epgsql:connection()}.

-record(player_cache,
        {player_id :: player_id(),
         client_pid :: pid(),
         username :: player_name(),
         email :: player_email()}).

-type player_cache() ::
    #player_cache{player_id :: player_id(),
                  client_pid :: pid(),
                  username :: player_name(),
                  email :: player_email()}.
