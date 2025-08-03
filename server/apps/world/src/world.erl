%%%-------------------------------------------------------------------
%% @doc World Server, takes care of boostrapping and maintaing items,
%%      resources and players around a given map.
%% @end
%%%-------------------------------------------------------------------
-module(world).

-behaviour(gen_server).

%% API
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-include("migrations.hrl").
-include("world_state.hrl").

-dialyzer({nowarn_function, [init/1]}).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the world, runs migrations and setup the main maps
%% @todo Conditionally remove test data if the rlx profile is "prod"
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> Result
    when Args :: list(),
         Result :: {ok, world_state()}.
init([]) ->
    % Setup a DB connection and bootstrap process state
    {ok, Conn} = database:connect_as_migraterl(),
    Dir = database_queries:get_root_dir(),
    logger:info("~nDIR=~p~n", [Dir]),
    % DB Migrations
    {ok, _} = init_db(Conn, Dir, main),
    {ok, _} = init_db(Conn, Dir, repeatable),
    {ok, _} = init_db(Conn, Dir, init_data),
    {ok, _} = init_db(Conn, Dir, test),
    % Start Worlds
    logger:info("Starting World Application...~n"),
    Pond = setup_map("Pond", 6, 6),
    State = #world_state{map = Pond, connection = Conn},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(term(), gen_server:from(), world_state()) -> Result
    when Result ::
             {reply, term(), world_state()} |
             {reply, term(), world_state(), timeout()} |
             {noreply, world_state()} |
             {noreply, world_state(), timeout()} |
             {stop, term(), term(), world_state()} |
             {stop, term(), world_state()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg, State) -> Result
    when Msg :: term(),
         State :: world_state(),
         NoReply :: {noreply, world_state()},
         NoReplyWithTimeOut :: {noreply, world_state(), timeout()},
         Stop :: {stop, term(), world_state()},
         Result :: NoReply | NoReplyWithTimeOut | Stop.
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info, State) -> Result
    when Info :: term(),
         State :: world_state(),
         NoReply :: {noreply, world_state()},
         NoReplyWithTimeOut :: {noreply, world_state(), timeout()},
         Stop :: {stop, term(), world_state()},
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
-spec setup_map(Name, Width, Height) -> Map
    when Name :: nonempty_string(),
         Width :: non_neg_integer(),
         Height :: non_neg_integer(),
         Map :: map_properties().
setup_map(Name, Width, Height) ->
    D = #map_dimension{width = Width, height = Height},
    #map_properties{name = Name, dimension = D}.

-spec init_db(Conn, Dir, Type) -> Result
    when Conn :: epgsql:connection(),
         Dir :: file:name_all(),
         Type :: migration_type(),
         Reason :: string(),
         Ok :: {ok, term()},
         Error :: {error, Reason},
         Result :: Ok | Error.
init_db(Conn, Dir, init_data) ->
    Suffix = ["database", "migrations", "init"],
    Path = filename:join([Dir | Suffix]),
    Options = #{repeatable => true},
    {ok, _} = migraterl:migrate(Conn, Path, Options),
    % Now populate the game's maps...
    MapPath = filename:join([Dir, "maps"]),
    ok = map_generator:create_map(Conn, MapPath, "Pond"),
    {ok, []};
init_db(Conn, Dir, Type) ->
    Suffix =
        case Type of
            main ->
                ["database", "migrations", "main"];
            repeatable ->
                ["database", "migrations", "repeatable"];
            test ->
                ["database", "migrations", "test"]
        end,
    Path = filename:join([Dir | Suffix]),
    logger:debug("[~p] MIGRATION PATH: ~p", [?MODULE, Path]),
    Options =
        case Type of
            main ->
                #{repeatable => false};
            _ ->
                #{repeatable => true}
        end,
    migraterl:migrate(Conn, Path, Options).
