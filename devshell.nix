{pkgs, packages ? [], erlangVersion, zigVersion, raylib, mkEnvVars, app_name}: 
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
      { name = "mmo"; }
      {
        name = app_name;
        user = app_name;
        pass = app_name;
      }
    ];
    port = 5432;
    listen_addresses = "127.0.0.1";
    initialScript = ''
      CREATE EXTENSION IF NOT EXISTS pg_stat_statements;
      CREATE USER admin SUPERUSER;
      ALTER USER admin PASSWORD 'admin';
      GRANT ALL PRIVILEGES ON DATABASE mmo to admin;
      ALTER USER ${app_name} CREATEROLE;
    '';
  };
}
