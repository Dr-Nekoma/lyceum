%%%-------------------------------------------------------------------
%% @doc Migration-related code
%% @end
%%%-------------------------------------------------------------------
-module(database_migration).

-export([migrate/1]).

%% Converts a relative directory path to an absolute path
to_absolute_path(RelativePath) ->
    case filelib:is_dir(RelativePath) of
        true ->
            AbsolutePath = filename:absname(RelativePath),
            {ok, AbsolutePath};
        false ->
            io:format("Invalid directory path: ~s~n", [RelativePath]),
            {error, invalid_directory}
    end.

all_sql_scripts_are_valid(List) ->
    lists:all(fun(E) ->
                 case E of
                     {ok, _} -> true;
                     {ok, _, _} -> true;
                     _ -> false
                 end
              end,
              List).

%% https://github.com/bearmug/erlang-pure-migrations?tab=readme-ov-file#postgresql-and-epgsqlepgsql
%% + This PR
%% https://github.com/bearmug/erlang-pure-migrations/pull/46
epgsql_query_fun(Conn) ->
    fun(Q) ->
       case epgsql:squery(Conn, Q) of
           {ok,
            [{column, <<"version">>, _, _, _, _, _, _, _},
             {column, <<"filename">>, _, _, _, _, _, _, _}],
            []} ->
               [];
           {ok,
            [{column, <<"version">>, _, _, _, _, _, _, _},
             {column, <<"filename">>, _, _, _, _, _, _, _}],
            Data} ->
               [{list_to_integer(binary_to_list(BinV)), binary_to_list(BinF)}
                || {BinV, BinF} <- Data];
           {ok, [{column, <<"max">>, _, _, _, _, _, _, _}], [{null}]} ->
               % TODO: The comment below is not mine
               % It has to be -1 or it will get an error during initialization
               % TODO (Mine): I've set this crap to zero and it somewhat works on a clean pg db...
               0;
           {ok, [{column, <<"max">>, _, _, _, _, _, _, _}], [{N}]} ->
               % The version number is stored in the int4 type and ranges from -2,147,483,648 to 2,147,483,647
               list_to_integer(binary_to_list(N));
           {ok,
            [{column, <<"version">>, _, _, _, _, _}, {column, <<"filename">>, _, _, _, _, _}],
            Data} ->
               [{list_to_integer(binary_to_list(BinV)), binary_to_list(BinF)}
                || {BinV, BinF} <- Data];
           {ok, [{column, <<"max">>, _, _, _, _, _}], [{null}]} -> -1;
           {ok, [{column, <<"max">>, _, _, _, _, _}], [{N}]} -> list_to_integer(binary_to_list(N));
           {ok, _, _} -> ok;
           {ok, _} -> ok;
           Default ->
               % Match multiple SQL statements in a script
               Res = all_sql_scripts_are_valid(Default),
               case Res of
                   true -> ok;
                   _ -> Default
               end
       end
    end.

migrate(Conn) ->
    io:format("Finding migration scripts... ~n"),
    {ok, RelativePath} = application:get_env(migrations_path),
    {ok, MigrationPath} = to_absolute_path(RelativePath),
    io:format("Migration Path: ~p~n", [MigrationPath]),
    TxFun = fun(F) -> epgsql:with_transaction(Conn, fun(_) -> F() end) end,
    QxFun = epgsql_query_fun(Conn),
    MigrationCall = pure_migrations:migrate(MigrationPath, TxFun, QxFun),
    io:format("Running DB migrations.~n"),
    case MigrationCall() of
        ok ->
            io:format("Migrations completed successfully.~n");
        {rollback, Details} ->
            io:format("Rollback: ~p~n", [Details]);
        {error, Type, Details} ->
            io:format("Generic Error: ~p~n~p~n", [Type, Details]);
        Other ->
            io:format("Warning: Something else is going on ~p~n", [Other])
    end.
