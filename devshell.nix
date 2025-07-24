{
  pkgs,
  packages ? [ ],
  erlangVersion,
  zigVersion,
  raylib,
  mkEnvVars,
  app_name,
  ...
}:
{
  packages = packages;

  languages.erlang = {
    enable = true;
  };

  languages.zig = {
    enable = true;
    package = zigVersion;
  };

  env = mkEnvVars pkgs erlangVersion raylib;

  scripts = {
    build.exec = "just build";
    server.exec = "just server";
    shell.exec = "just shell";
    client.exec = "just client";
    client-release.exec = "just client-release";
    db-up.exec = "just db-up";
    db-down.exec = "just db-down";
    db-reset.exec = "just db-reset";
    pg-con.exec = "just db";
  };

  enterShell = ''
    echo "Starting Development Environment..."
  '';

  enterTest = ''
    # Building and testing
    just test

    # Linting and formatting

    # Build dioxus desktop example
  '';

  services.postgres = {
    enable = true;
    package = pkgs.postgresql_17;
    extensions = ext: [
      ext.periods
      ext.omnigres
    ];
    initdbArgs = [
      "--locale=C"
      "--encoding=UTF8"
    ];
    settings = {
      shared_preload_libraries = "pg_stat_statements";
      # pg_stat_statements config, nested attr sets need to be
      # converted to strings, otherwise postgresql.conf fails
      # to be generated.
      compute_query_id = "on";
      "pg_stat_statements.max" = 10000;
      "pg_stat_statements.track" = "all";
    };
    initialDatabases = [
      {
        # Database Name
        name = app_name;
        # User who owns it
        user = app_name;
        # A password used for local development
        pass = app_name;
        initialSQL = builtins.readFile ./server/database/local/init.sql;
      }
    ];
    port = 5432;
    listen_addresses = "127.0.0.1";
    initialScript = ''
      -- The app user
      ALTER USER ${app_name} CREATEROLE;
    '';
  };

  # TODO: enable this
  #git-hooks.hooks = {
  #  nixfmt-rfc-style.enable = true;
  #};
}
