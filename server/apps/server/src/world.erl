%%%-------------------------------------------------------------------
%% @doc World Server
%% @end
%%%-------------------------------------------------------------------
-module(world).

%% API
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(MAIN_MODULE_NAME, server).

-include("migrations.hrl").
-include("server_state.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the world, runs migrations and setup the main maps
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Result when
      Args :: list(),
      Result :: {ok, server_state()}.
init([]) ->
    % Setup a DB connection and bootstrap process state
    {ok, Conn} = database:connect(),
    LibDir = filename:absname(code:lib_dir(?MAIN_MODULE_NAME)),
    Dir = filename:dirname(filename:dirname(LibDir)),
    % DB Migrations
    {ok, _} = init_db(Conn, Dir, main),
    {ok, _} = init_db(Conn, Dir, repeatable),
    {ok, _} = init_db(Conn, Dir, init_data),
    % Start Worlds
    logger:info("Starting World Application...~n"),
    State = #server_state{connection = Conn, pid = self(), table = []},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(term(), gen_server:from(), server_state()) -> Result when
     Result :: 
        {reply, term(), server_state()}
        | {reply, term(), server_state(), timeout()}
        | {noreply, server_state()}
        | {noreply, server_state(), timeout()}
        | {stop, term(), term(), server_state()} 
        | {stop, term(), server_state()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg, State) -> Result when
      Msg :: term(),
      State :: server_state(),
      NoReply :: {noreply, server_state()},
      NoReplyWithTimeOut :: {noreply, server_state(), timeout()},
      Stop :: {stop, term(), server_state()},
      Result :: NoReply | NoReplyWithTimeOut | Stop.
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info, State) -> Result when
      Info :: term(),
      State :: server_state(),
      NoReply :: {noreply, server_state()},
      NoReplyWithTimeOut :: {noreply, server_state(), timeout()},
      Stop :: {stop, term(), server_state()},
      Result :: NoReply | NoReplyWithTimeOut | Stop.
handle_info(Info, State) ->
    logger:info("[~p] INFO: ~p~n", [?SERVER, Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec init_db(Conn, Dir, Type) -> Result when
      Conn :: epgsql:connection(),
      Dir :: file:name_all(),
      Type :: migration_type(),
      Reason :: string(),
      Ok :: {ok, term()},
      Error :: {error, Reason},
      Result :: Ok | Error.
init_db(Conn, Dir, main) ->
    Suffix = ["migrations", "main"],
    Path = filename:join([Dir | Suffix]),
    Options = #{repeatable => false},
    migraterl:migrate(Conn, Path, Options);
init_db(Conn, Dir, repeatable) ->
    Suffix = ["migrations", "repeatable"],
    Path = filename:join([Dir | Suffix]),
    Options = #{repeatable => true},
    migraterl:migrate(Conn, Path, Options);
init_db(Conn, Dir, init_data) ->
    Suffix = ["migrations", "init"],
    Path = filename:join([Dir | Suffix]),
    Options = #{repeatable => true},
    {ok, _} = migraterl:migrate(Conn, Path, Options),
    % Now populate the game's maps...
    MapPath = filename:join([Dir, "maps"]),
    ok = map_generator:create_map(Conn, MapPath, "Pond"),
    {ok, []}.
