{
  pkgs,
  packages ? [ ],
  erlangVersion,
  zigVersion,
  raylib,
  mkEnvVars,
  app_name,
  system,
  ...
}:
{
  packages = packages;

  languages.erlang = {
    enable = true;
    package = erlangVersion;
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
  '';

  services.postgres = {
    enable = true;
    package = pkgs.postgresql_18;
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
      session_preload_libraries = "auto_explain";
      "auto_explain.log_min_duration" = 150;
      "auto_explain.log_analyze" = true;
      log_min_duration_statement = 0;
      log_statement = "all";
      # pg_stat_statements config, nested attr sets need to be
      # converted to strings, otherwise postgresql.conf fails
      # to be generated.
      compute_query_id = "on";
      "pg_stat_statements.max" = 10000;
      "pg_stat_statements.track" = "all";
      # Adjust shared buffers
      shared_buffers = "1GB";
      # Increase work memory for large operations
      work_mem = "16MB";
      # Enable huge pages if available
      huge_pages = "try";
      # Adjust I/O concurrency settings
      effective_io_concurrency = 16;
      maintenance_io_concurrency = 16;
    }
    // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
      # Async IO, io_uring or workers
      # For io_uring method (Linux only, requires liburing)
      io_method = "io_uring";
    }
    // pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
      # in case "io_uring" is not available
      io_method = "worker";
      # For systems with many CPU cores and high I/O latency
      io_workers = 8;
      # For smaller systems or fast local storage
      # io_workers = 2;
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
      ALTER USER ${app_name} SUPERUSER CREATEROLE;

      CREATE USER migrations
      WITH LOGIN SUPERUSER CREATEROLE PASSWORD 'migrations';

      CREATE USER admin
      WITH LOGIN SUPERUSER CREATEROLE PASSWORD 'admin';

      CREATE USER application WITH LOGIN PASSWORD 'application';
      CREATE USER lyceum_auth WITH LOGIN PASSWORD 'lyceum_auth';
      CREATE USER mnesia WITH LOGIN PASSWORD 'mnesia';
    '';
  };
}
