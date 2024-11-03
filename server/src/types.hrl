-type email_pid() :: {Email :: string(), Pid :: pid()}.

-record(state, {connection :: epgsql:connection(), pid :: pid(), table :: [email_pid()]}).

-type state() ::
    #state{connection :: epgsql:connection(),
           pid :: pid(),
           table :: [email_pid()]}.
