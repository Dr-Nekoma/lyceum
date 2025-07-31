#!/usr/bin/env escript

main(_) ->
    application:load(world),
    application:set_env(world, is_shell, true),
    ok.
