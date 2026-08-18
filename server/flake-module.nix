{
  perSystem =
    {
      pkgs,
      lib,
      lyceum,
      self',
      ...
    }:
    let
      inherit (lyceum) app_name app_version erlangVersion;
      erl_app = "server";

      # rebar3Relx/buildRebar3 have no `rebar3` argument, each build
      # instead constructs `rebar3WithPlugins { plugins = buildPlugins; }`
      # internally. To make a plugin globally available we therefore set
      # `buildPlugins` on the release and on every dependency build.
      # pc (port_compiler) is required by some of our dependencies.
      buildPlugins = with pkgs.beamPackages; [ pc ];

      # Strip .nix files so edits to this module don't invalidate the
      # server build cache.
      src = lib.cleanSourceWith {
        src = ./.;
        filter = name: type: (lib.cleanSourceFilter name type) && !(lib.hasSuffix ".nix" (baseNameOf name));
      };
    in
    {
      # nix build .#server
      packages.server =
        let
          deps = import ./rebar-deps.nix {
            inherit (pkgs) fetchHex fetchFromGitHub fetchgit;
            builder = args: pkgs.beamPackages.buildRebar3 (args // { inherit buildPlugins; });
          };

          release = pkgs.beamPackages.rebar3Relx {
            inherit src;
            pname = erl_app;
            version = app_version;
            root = ./.;
            releaseType = "release";
            profile = "prod";
            plugins = [
              pkgs.beamPackages.pc
            ];
            include = [
              "rebar.config"
            ];
            buildInputs = (
              with pkgs;
              [
                coreutils
                gawk
                gnugrep
                openssl
              ]
              ++ lib.optional stdenv.isLinux [
                liburing
              ]
            );
            beamDeps = builtins.attrValues deps;
            buildPhase = ''
              runHook preBuild
              HOME=. DEBUG=1 rebar3 as prod release --relname ${app_name}
              runHook postBuild
            '';
          };
        in
        # The release reads its node name, layers and peers from the
        # environment: `config/{sys.config,vm.args}.src` are expanded at
        # *start* time. relx writes the expanded files next to the .src
        # ones, which here means inside the store, so point it at a
        # writable directory instead. Anything already exported wins, so
        # a deployment can put the generated config wherever it likes.
        #
        # rebar3Relx defines its own postInstall, hence overrideAttrs.
        release.overrideAttrs (previous: {
          nativeBuildInputs = (previous.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];
          postInstall = (previous.postInstall or "") + ''
            wrapProgram $out/bin/${app_name} \
              --run 'export RELX_OUT_FILE_PATH="''${RELX_OUT_FILE_PATH:-''${XDG_RUNTIME_DIR:-/tmp}/${app_name}}"' \
              --run 'mkdir -p "$RELX_OUT_FILE_PATH"'
          '';
        });

      # nix build .#dockerImage
      packages.dockerImage = pkgs.dockerTools.buildLayeredImage {
        name = erl_app;
        tag = "latest";
        created = "now";
        contents = [
          self'.packages.server
        ];
        config = {
          Volumes = {
            "/opt/${erl_app}/etc" = { };
            "/opt/${erl_app}/data" = { };
            "/opt/${erl_app}/log" = { };
          };
          WorkingDir = "/opt/${erl_app}";
          Cmd = [
            "${self'.packages.server}/bin/${app_name}"
            "foreground"
          ];
          Env = [
            "ERL_DIST_PORT=8080"
            "ERL_AFLAGS=\"-kernel shell_history enabled\""
            # The image defaults to an all-in-one node. Split deployments
            # override these three: LYCEUM_NODE_NAME must stay
            # `lyceum_server` on whichever node hosts the frontend, since
            # that name is what the client dials.
            "LYCEUM_NODE_NAME=lyceum_server"
            "LYCEUM_NODE_TYPES=frontend,logic,service"
            "LYCEUM_PEERS="
          ];
          ExposedPorts = {
            "4369/tcp" = { };
            "4369/udp" = { };
            "8080/tcp" = { };
            "8080/udp" = { };
          };
        };
      };

      # nix run .#server -- foreground
      apps.server = {
        type = "app";
        program = "${self'.packages.server}/bin/${app_name}";
      };

      # Server-side dev-shell contributions, merged into the shared
      # devenv shell defined at the top level.
      devenv.shells.default = {
        languages.erlang = {
          enable = true;
          package = erlangVersion;
        };

        packages = with pkgs; [
          erlang-language-platform
          erlfmt
          postgresql
          sqls
          watchman
        ];

        env = {
          # https://www.erlang.org/doc/man/kernel_app.html
          ERL_AFLAGS = "-kernel shell_history enabled";
          ERL_INCLUDE_PATH = "${erlangVersion}/lib/erlang/usr/include";
          # Devenv sets this to something else
          # https://www.postgresql.org/docs/7.0/libpq-envars.htm
          PGHOST = "127.0.0.1";
        };

        scripts = {
          server.exec = "just server";
          shell.exec = "just shell";
          db-up.exec = "just db-up";
          db-down.exec = "just db-down";
          db-reset.exec = "just db-reset";
          pg-con.exec = "just db";
        };

        services.postgres = {
          enable = true;
          package = pkgs.postgresql_18;
          extensions = ext: [
            ext.pg_cron
            ext.omnigres
          ];
          initdbArgs = [
            "--locale=C"
            "--encoding=UTF8"
          ];
          settings = {
            shared_preload_libraries = pkgs.lib.concatStringsSep "," [
              "auto_explain"
              "pg_cron"
              "pg_stat_statements"
            ];
            session_preload_libraries = "auto_explain";
            "auto_explain.log_min_duration" = 150;
            "auto_explain.log_analyze" = true;
            log_min_duration_statement = 0;
            log_statement = "all";
            # pg_cron's background worker only runs in a single database.
            # Point it at the app's dev DB so `CREATE EXTENSION pg_cron`
            # and cron.schedule() operate on app tables.
            "cron.database_name" = "${app_name}";
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
          // lib.optionalAttrs pkgs.stdenv.isLinux {
            # Async IO, io_uring or workers
            # For io_uring method (Linux only, requires liburing)
            io_method = "io_uring";
          }
          // lib.optionalAttrs pkgs.stdenv.isDarwin {
            # in case "io_uring" is not available
            io_method = "worker";
            # For systems with many CPU cores and high I/O latency
            io_workers = 8;
            # For smaller systems or fast local storage
            # io_workers = 2;
          };
          initialDatabases = [
            {
              name = app_name;
              user = app_name;
              pass = app_name;
              initialSQL = builtins.readFile ./database/local/init.sql;
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
      };
    };
}
