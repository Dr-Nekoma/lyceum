-module(lyceum_cluster).
-moduledoc """
Cluster membership and service discovery for every Lyceum node.

Lyceum is split into three layers, each deployed as its own node type:

- `frontend`: client sessions, authentication, the web application
- `logic`: player state machines, the world
- `service`: PostgreSQL access and third-party integrations

Calls only ever travel *downward* (`frontend -> logic -> service`).
This module is what makes that traversal possible without any node
hardcoding the name of another node.

## Registration

Processes that serve a layer register themselves in a process group
with `join/1`, and callers reach one with `call/3`:

```erlang
%% in the callee, at init/1
ok = lyceum_cluster:join(service),

%% in the caller
lyceum_cluster:call(service, Request, #{
    timeout => 5000,
    unavailable => no_service,
    timed_out => service_timeout
}).
```

`call/3` is the only thing that should be doing cross-layer requests:
it holds the retry and error-mapping policy, so callers do not each
grow their own copy of it. `pick/1` is still available for the rarer
cases that need the pid itself.

Groups live in the `pg` scope `lyceum`. `pg` is used rather than
`global` deliberately: `global` serialises every registration through
a cluster-wide lock, which is the wrong shape for per-player and
per-worker registration.

## Boot invariant

`pg`'s scope is started by `lyceum_cluster_sup` before anything else
in the tree, and every other Lyceum application depends on
`lyceum_cluster`. Lookups therefore assume the scope exists and will
crash if it does not, since that can only mean a broken release
manifest.
""".

-export([node_types/0, hosts_layer/1, peers/0, scope/0]).
-export([join/1, join/2, leave/1, leave/2]).
-export([members/1, local_members/1, pick/1]).
-export([call/2, call/3]).
-export([nodes_of_type/1]).

-export_type([node_type/0, group/0, call_opts/0]).

-define(SCOPE, lyceum).
-define(DEFAULT_TIMEOUT, 5000).

-doc "A layer of the system. Set per release in `sys.config`.".
-type node_type() :: frontend | logic | service.

-doc """
A process group name.

`frontend | logic | service` are the service groups a layer's workers
join. `{node_type, T}` is joined by each node's connector and is how
`nodes_of_type/1` enumerates the cluster. Other tuples are free for
callers to use, for example `{player, PlayerId}`.
""".
-type group() :: node_type() | {node_type, node_type()} | tuple().

-doc """
How a caller wants `call/3` to answer when a layer is not usable.

`unavailable` is returned when the group is empty or the callee is
gone, `timed_out` when it accepted the request but did not answer in
`timeout` milliseconds. They are separate because callers act on them
differently: an unavailable layer is worth reporting to the player
right away, while a timeout may have been applied anyway.
""".
-type call_opts() :: #{
    timeout => timeout(),
    unavailable => term(),
    timed_out => term()
}.

%%%===================================================================
%%% Configuration
%%%===================================================================
-doc """
The layers this node hosts.

A list rather than a single atom because a node hosting more than one
layer is a supported deployment, not a special case: `just server`
runs all three in one VM so a single command still gives you a working
game.

Both the Erlang form (`[frontend, logic]`, from `sys.config`) and the
string form (`"frontend,logic"`, what `$LYCEUM_NODE_TYPES` expands to
in `sys.config.src`) are accepted.

Raises `{missing_config, node_types}` when unset and
`{invalid_config, node_types, Value}` on anything unparseable. There is
no default: guessing which layers a node hosts would let a
misconfigured node serve the wrong traffic, which is worse than not
booting.
""".
-spec node_types() -> [node_type(), ...].
node_types() ->
    case application:get_env(lyceum_cluster, node_types) of
        {ok, Value} ->
            parse_node_types(Value);
        undefined ->
            error({missing_config, node_types})
    end.

-doc """
Whether this node hosts `Type`.

Note that this answers "was this node configured to run that layer",
not "is that layer reachable". Use `pick/1` for the latter, a layer is
only usable once its workers have actually registered.
""".
-spec hosts_layer(node_type()) -> boolean().
hosts_layer(Type) ->
    lists:member(Type, node_types()).

-doc """
The statically configured nodes this one tries to connect to.

Connectivity is intentionally config-driven rather than discovered:
nodes run with `connect_all false`, so the only links that exist are
the ones this list asks for. Peers point downward
(frontend -> logic -> service), which is what keeps a frontend node
from ever seeing the `service` groups.

Like `node_types/0` this takes either the Erlang form
(`['lyceum_logic@host']`) or the string form
(`"lyceum_logic@host,lyceum_svc@host"`) coming from `$LYCEUM_PEERS`.
An empty list is legitimate: an all-in-one node has nothing to connect
to, and in tests the topology is wired externally.
""".
-spec peers() -> [node()].
peers() ->
    parse_peers(application:get_env(lyceum_cluster, peers, [])).

-doc "The `pg` scope every Lyceum group lives in.".
-spec scope() -> atom().
scope() ->
    ?SCOPE.

%%%===================================================================
%%% Group membership
%%%===================================================================
-doc "Joins the calling process to `Group`.".
-spec join(group()) -> ok.
join(Group) ->
    join(Group, self()).

-doc """
Joins `Pid` to `Group`.

`pg` monitors the process, so membership is dropped automatically when
it dies. There is no need to leave on the way out of a normal exit.
""".
-spec join(group(), pid()) -> ok.
join(Group, Pid) ->
    pg:join(?SCOPE, Group, Pid).

-doc "Removes the calling process from `Group`.".
-spec leave(group()) -> ok | not_joined.
leave(Group) ->
    leave(Group, self()).

-doc "Removes `Pid` from `Group`.".
-spec leave(group(), pid()) -> ok | not_joined.
leave(Group, Pid) ->
    pg:leave(?SCOPE, Group, Pid).

-doc """
Every member of `Group` across the whole cluster.

`pg` is eventually consistent: shortly after a netsplit heals, or just
after a remote process dies, this can still list a pid that is already
gone. Callers must cope with that, which is why `pick/1`'s result is
only ever used inside a call that handles `noproc`.
""".
-spec members(group()) -> [pid()].
members(Group) ->
    pg:get_members(?SCOPE, Group).

-doc "Members of `Group` running on this node.".
-spec local_members(group()) -> [pid()].
local_members(Group) ->
    pg:get_local_members(?SCOPE, Group).

-doc """
Picks one member of `Group`, or `{error, no_service}` when empty.

Local members win over remote ones, so an all-in-one development node
never pays for a network hop and a healthy node prefers to keep work
on itself. Beyond that the choice is random, which is enough to spread
load across interchangeable nodes of the same type.
""".
-spec pick(group()) -> pid() | {error, no_service}.
pick(Group) ->
    case local_members(Group) of
        [] ->
            random_member(members(Group));
        Local ->
            random_member(Local)
    end.

%%%===================================================================
%%% Calling
%%%===================================================================
-doc "Equivalent to `call(Group, Request, #{})`.".
-spec call(group(), term()) -> term().
call(Group, Request) ->
    call(Group, Request, #{}).

-doc """
Sends `Request` to a member of `Group` and returns its reply.

This is the single place where "reach another layer" is implemented,
so every caller gets the same behaviour without repeating it:

- an empty group answers `{error, Unavailable}` rather than raising,
  because a layer being down is an ordinary condition here, not a bug;
- a member that turns out to be dead costs exactly one re-pick. `pg`
  is eventually consistent, so a pid can be stale by the time it is
  called; a second `noproc` means the whole group view is stale and
  there is nothing to gain from trying again;
- a callee that never answers gives `{error, TimedOut}`, and a node
  that vanished mid-call gives `{error, Unavailable}`.

Anything else the callee replies is passed through untouched, errors
included: `call/3` only translates failures of the *transport*, never
of the request.
""".
-spec call(group(), term(), call_opts()) -> term().
call(Group, Request, Opts) ->
    case pick(Group) of
        {error, no_service} ->
            {error, unavailable(Opts)};
        Pid ->
            try_call(Pid, Group, Request, Opts, _Retry = true)
    end.

-spec try_call(pid(), group(), term(), call_opts(), boolean()) -> term().
try_call(Pid, Group, Request, Opts, Retry) ->
    try
        gen_server:call(Pid, Request, maps:get(timeout, Opts, ?DEFAULT_TIMEOUT))
    catch
        exit:{noproc, _} when Retry ->
            case pick(Group) of
                {error, no_service} ->
                    {error, unavailable(Opts)};
                NewPid ->
                    try_call(NewPid, Group, Request, Opts, false)
            end;
        exit:{noproc, _} ->
            {error, unavailable(Opts)};
        exit:{timeout, _} ->
            {error, maps:get(timed_out, Opts, timeout)};
        exit:{{nodedown, _}, _} ->
            {error, unavailable(Opts)};
        exit:{shutdown, _} ->
            {error, unavailable(Opts)}
    end.

-spec unavailable(call_opts()) -> term().
unavailable(Opts) ->
    maps:get(unavailable, Opts, no_service).

-doc """
Every node of a given type currently visible from here, including this
one when it matches.

Used where a layer genuinely needs the node list rather than a single
worker, for instance to decide which nodes hold a replica of the
session table.
""".
-spec nodes_of_type(node_type()) -> [node()].
nodes_of_type(Type) ->
    lists:usort([node(Pid) || Pid <- members({node_type, Type})]).

%%%===================================================================
%%% Configuration parsing
%%%===================================================================
-spec parse_node_types(term()) -> [node_type(), ...].
parse_node_types(Value) ->
    Types =
        case tokens(Value) of
            {ok, Tokens} -> [existing_atom(Token) || Token <- Tokens];
            not_a_string -> Value
        end,
    case is_list(Types) andalso Types =/= [] andalso lists:all(fun is_node_type/1, Types) of
        true -> lists:usort(Types);
        false -> error({invalid_config, node_types, Value})
    end.

-spec parse_peers(term()) -> [node()].
parse_peers(Value) ->
    case tokens(Value) of
        {ok, Tokens} ->
            [to_node(Token, Value) || Token <- Tokens];
        not_a_string when is_list(Value) ->
            case lists:all(fun is_atom/1, Value) of
                true -> Value;
                false -> error({invalid_config, peers, Value})
            end;
        not_a_string ->
            error({invalid_config, peers, Value})
    end.

-doc """
Splits a comma-separated configuration string into trimmed, non-empty
tokens, or says the value was not a string at all so the caller can
fall back to the Erlang term form.

Note that `[]` reads as the empty string here, which is the same thing
either way: nothing was configured.
""".
-spec tokens(term()) -> {ok, [string()]} | not_a_string.
tokens(Value) when is_binary(Value) ->
    tokens(binary_to_list(Value));
tokens(Value) when is_list(Value) ->
    case lists:all(fun is_char/1, Value) of
        true ->
            Trimmed = [string:trim(Token) || Token <- string:split(Value, ",", all)],
            {ok, [Token || Token <- Trimmed, Token =/= ""]};
        false ->
            not_a_string
    end;
tokens(_Value) ->
    not_a_string.

-spec is_char(term()) -> boolean().
is_char(C) when is_integer(C), C >= 0, C =< 16#10FFFF -> true;
is_char(_) -> false.

-doc """
Keeps unknown tokens as strings rather than minting atoms for them:
they fail validation just the same, and a typo in a config file should
not be able to grow the atom table.
""".
-spec existing_atom(string()) -> atom() | string().
existing_atom(Token) ->
    try
        list_to_existing_atom(Token)
    catch
        error:badarg -> Token
    end.

-doc """
Node names are the one place new atoms have to be created: a peer this
node has never talked to has no atom yet. The `name@host` shape is
checked first so a malformed entry fails loudly instead of silently
becoming a node nobody can connect to.
""".
-spec to_node(string(), term()) -> node().
to_node(Token, Value) ->
    case string:split(Token, "@", all) of
        [[_ | _], [_ | _]] -> list_to_atom(Token);
        _ -> error({invalid_config, peers, Value})
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec is_node_type(term()) -> boolean().
is_node_type(frontend) -> true;
is_node_type(logic) -> true;
is_node_type(service) -> true;
is_node_type(_) -> false.

-spec random_member([pid()]) -> pid() | {error, no_service}.
random_member([]) ->
    {error, no_service};
random_member([Pid]) ->
    Pid;
random_member(Pids) ->
    lists:nth(rand:uniform(length(Pids)), Pids).
