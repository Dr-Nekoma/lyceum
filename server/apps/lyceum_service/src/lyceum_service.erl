-module(lyceum_service).
-moduledoc """
Caller-side facade for the service layer.

Every operation that touches PostgreSQL or the session cache goes
through this module. The work executes in a `lyceum_service_worker`
reached with `lyceum_cluster:call(service, ...)`, on an all-in-one node the
picked worker is local (`pick/1` prefers local members), so there is
no network hop, and the code path is identical in every topology.
There is deliberately no co-located short-circuit: one path keeps
transaction affinity, backpressure and error semantics the same
whether or not the service layer is remote.

Errors introduced by distribution are normalized:

- `{error, no_service}`: no service worker is reachable (empty pg
  group, or the node died mid-call).
- `{error, service_timeout}`: a worker was reached but did not answer
  within `call_timeout`.

Both are safe to surface to game code, which treats them like any
other operation error.
""".

-export([check_user/1, login_session/1, logout_session/1]).
-export([
    list_characters/1,
    join_map/1,
    update_character/1,
    harvest_resource/1,
    exit_map/1,
    logout_player/1
]).

-export_type([error/0]).

-include("player_state.hrl").

-type error() :: {error, no_service | service_timeout | term()}.

-doc "Validates credentials against the player registry.".
-spec check_user(#{username := _, password := _, _ => _}) ->
    {ok, {player_id(), player_email()}} | error().
check_user(Request) ->
    call({check_user, Request}).

-doc """
Registers a login in the session cache.

Returns the (possibly merged) cache record plus the client pid of the
previous session for the same player, or `undefined` when there was
none. The single service-side cache serializes concurrent logins, so
the returned previous pid is the duplicate-login arbiter.
""".
-spec login_session(player_cache()) ->
    {ok, player_cache(), pid() | undefined} | error().
login_session(Cache) ->
    call({login_session, Cache}).

-doc "Removes a player's session from the cache.".
-spec logout_session(player_id()) -> ok | error().
logout_session(PlayerId) ->
    call({logout_session, PlayerId}).

-doc "Lists every character owned by a player.".
-spec list_characters(map()) -> {ok, [map()]} | error().
list_characters(Request) ->
    call({list_characters, Request}).

-doc """
Activates a character and fetches its data plus the map it is on.
One service round trip for the whole join.
""".
-spec join_map(map()) -> {ok, #{character := map(), map := map()}} | error().
join_map(Request) ->
    call({join_map, Request}).

-doc """
Persists a character update and returns the players near it.
One service round trip for both.
""".
-spec update_character(map()) -> {ok, [map()]} | error().
update_character(Request) ->
    call({update_character, Request}).

-doc "Harvests a resource, returning the inventory/resource deltas.".
-spec harvest_resource(map()) -> {ok, map()} | error().
harvest_resource(Request) ->
    call({harvest_resource, Request}).

-doc "Deactivates a character when it leaves a map.".
-spec exit_map(#{name := _, email := _, username := _}) -> ok | error().
exit_map(Request) ->
    call({exit_map, Request}).

-doc "Deactivates a character and drops the player's session.".
-spec logout_player(#{name := _, email := _, username := _, player_id := _}) ->
    ok | error().
logout_player(Request) ->
    call({logout_player, Request}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Routing, retry and the mapping of a broken connection onto an error
%% value all live in lyceum_cluster:call/3; the only thing this layer
%% decides is what its own failures are called and how long it waits.
-spec call(term()) -> term().
call(Request) ->
    lyceum_cluster:call(service, Request, #{
        timeout => application:get_env(lyceum_service, call_timeout, 5000),
        unavailable => no_service,
        timed_out => service_timeout
    }).
