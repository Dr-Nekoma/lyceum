{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils = {
      url = "github:numtide/flake-utils/v1.0.0";
    };

    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    zig2nix.url = "github:Cloudef/zig2nix";
  };

  outputs =
    {
      self,
      nixpkgs,
      devenv,
      flake-utils,
      zig2nix,
      ...
    }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
        getErlangLibs =
          erlangPkg:
          let
            erlangPath = "${erlangPkg}/lib/erlang/lib/";
            dirs = builtins.attrNames (builtins.readDir erlangPath);
            interfaceVersion = builtins.head (
              builtins.filter (s: builtins.substring 0 13 s == "erl_interface") dirs
            );
            interfacePath = erlangPath + interfaceVersion;
          in
          {
            path = erlangPath;
            dirs = dirs;
            interface = {
              version = interfaceVersion;
              path = interfacePath;
            };
          };

        # Environment-specific packages
        linuxPkgs = with pkgs; [
          inotify-tools
          xorg.libX11
          xorg.libX11.dev
          xorg.libXrandr
          xorg.libXinerama
          xorg.libXcursor
          xorg.libXi
          libGL
          libpulseaudio
        ];
        darwinPkgs = with pkgs.darwin.apple_sdk.frameworks; [
          CoreFoundation
          CoreServices
        ];

        # Erlang
        erlangLatest = pkgs.erlang_27;
        erlangLibs = getErlangLibs erlangLatest;
        erl_app = "server";

        # Zig shit (Incomplete)
        zigLatest = pkgs.zig;
        raylib = pkgs.raylib;
        env = zig2nix.outputs.zig-env.${system} {
          #zig = zig2nix.outputs.packages.${system}.zig.master.bin;
          customRuntimeLibs = [
            pkgs.pkg-config
            erlangLibs
            raylib
          ] 
            ++ pkgs.lib.optionals pkgs.stdenv.isLinux linuxPkgs
            ++ pkgs.lib.optionals pkgs.stdenv.isLinux darwinPkgs;
          customRuntimeDeps = [
            erlangLibs
            raylib
          ]
            ++ pkgs.lib.optionals pkgs.stdenv.isLinux linuxPkgs
            ++ pkgs.lib.optionals pkgs.stdenv.isLinux darwinPkgs;
          enableOpenGL = true;
          enableWayland = true;
          enableX11 = true;
        };
        system-triple = env.lib.zigTripleFromString system;

        mkEnvVars = pkgs: erlangLatest: erlangLibs: raylib: {
          LOCALE_ARCHIVE = pkgs.lib.optionalString pkgs.stdenv.isLinux "${pkgs.glibcLocales}/lib/locale/locale-archive";
          LANG = "en_US.UTF-8";
          # https://www.erlang.org/doc/man/kernel_app.html
          ERL_AFLAGS = "-kernel shell_history enabled";
          ERL_INCLUDE_PATH = "${erlangLatest}/lib/erlang/usr/include";
          # Devenv sets this to something else
          # https://www.postgresql.org/docs/7.0/libpq-envars.htm
          PGHOST = "127.0.0.1";
        };
      in
      {
        # nix build
        packages = rec {
          devenv-up = self.devShells.${system}.default.config.procfileScript;

          # Leverages nix to build the erlang backend release
          # nix build .#server
          server =
            let
              deps = import ./server/rebar-deps.nix { 
                inherit (pkgs) fetchHex fetchFromGitHub fetchgit;
                builder = pkgs.beamPackages.buildRebar3;
              };
            in
            pkgs.beamPackages.rebar3Relx {
              pname = erl_app;
              version = "0.1.0";
              root = ./server;
              src = pkgs.lib.cleanSource ./server;
              releaseType = "release";
              profile = "prod";
              include = [
                "rebar.config"
              ];
              beamDeps = builtins.attrValues deps;
              buildPhase = ''
                runHook preBuild
                HOME=. DEBUG=1 rebar3 as prod release --relname server
                runHook postBuild
              '';
            };

          # nix build .#dockerImage
          dockerImage = pkgs.dockerTools.buildLayeredImage {
            name = erl_app;
            tag = "latest";
            created = "now";
            # This will copy the compiled erlang release to the image
            contents = [
              server
              pkgs.coreutils
              pkgs.gawk
              pkgs.gnugrep
              pkgs.openssl
            ];
            config = {
              Volumes = {
                "/opt/${erl_app}/etc" = {};
                "/opt/${erl_app}/data" = {};
                "/opt/${erl_app}/log" = {};
              };
              WorkingDir = "/opt/${erl_app}";
              Cmd = [
                "${server}/bin/${erl_app}"
                "foreground"
              ];
              Env = [
                "ERL_DIST_PORT=8080"
                "ERL_AFLAGS=\"-kernel shell_history enabled\""
                "NODE_NAME=${erl_app}"
              ];
              ExposedPorts = {
                "4369/tcp" = {};
                "4369/ucp" = {};
                "8080/tcp" = {};
                "8080/udp" = {};
              };
            };
          };

          # TODO: Finish this, it's incomplete
          # Leverages nix to build the zig-based client
          # nix build .#client
          packages.target = env.pkgs.lib.genAttrs env.lib.allTargetTriples (target:
            env.packageForTarget target ({
                src = env.pkgs.lib.cleanSource ./client;

                nativeBuildInputs = with env.pkgs; [
                  raylib
                ];
                buildInputs = with env.pkgsForTarget target; [];

                # Smaller binaries and avoids shipping glibc.
                zigPreferMusl = true;

                # This disables LD_LIBRARY_PATH mangling, binary patching etc...
                # The package won't be usable inside nix.
                zigDisableWrap = true;
              } // env.lib.optionalAttrs (!env.lib.pathExists ./client/build.zig.zon) {
                pname = "lyceum-client";
                version = "0.0.1";
              }));

          packages.client = packages.target.${system-triple}.override {
            # Prefer nix friendly settings.
            zigPreferMusl = false;
            zigDisableWrap = false;
          };
        };

        # nix run
        apps = {
          packages.default = env.lib.packages.target.${system-triple}.override {
            # Prefer nix friendly settings.
            zigPreferMusl = false;
            zigDisableWrap = false;
          };

          # nix run .#build
          apps.build = env.app [ ] "zig build -- \"$@\"";

          # nix run .#test
          apps.test = env.app [ ] "zig build test -- \"$@\"";
        };

        devShells =
          {
            # `nix develop .#ci`
            # reduce the number of packages to the bare minimum needed for CI
            ci = pkgs.mkShell {
              env = mkEnvVars pkgs erlangLatest erlangLibs raylib;
              buildInputs = with pkgs; [
                erlangLatest
                just
                rebar3
                rsync
                zigLatest
                raylib
              ];
            };

            # `nix develop .#server`
            server = pkgs.mkShell {
              buildInputs = with pkgs; [
                erlangLatest
                rebar3
              ];
            };

            # `nix develop`
            default = devenv.lib.mkShell {
              inherit inputs pkgs;
              modules = [
                (
                  { pkgs, lib, ... }:
                  {
                    packages =
                      with pkgs;
                      [
                        just
                        raylib
                        sqls
                      ]
                      ++ lib.optionals stdenv.isLinux (linuxPkgs)
                      ++ lib.optionals stdenv.isDarwin darwinPkgs;

                    languages.erlang = {
                      enable = true;
                      package = erlangLatest;
                    };

                    languages.zig = {
                      enable = true;
                      package = zigLatest;
                    };

                    env = mkEnvVars pkgs erlangLatest erlangLibs raylib;

                    scripts = {
                      build.exec = "just build";
                      server.exec = "just server";
                      client.exec = "just client";
                      client-release.exec = "just client-release";
                      db-up.exec = "just db-up";
                      db-down.exec = "just db-down";
                      db-reset.exec = "just db-reset";
                    };

                    enterShell = ''
                      echo "Starting Development Environment..."
                    '';

                    services.postgres = {
                      enable = true;
                      package = pkgs.postgresql_17;
                      extensions = ext: [
                        ext.periods
                      ];
                      initdbArgs = ["--locale=C" "--encoding=UTF8"];
                      settings = {
                        shared_preload_libraries = "pg_stat_statements";
                        # pg_stat_statements config, nested attr sets need to be
                        # converted to strings, otherwise postgresql.conf fails
                        # to be generated.
                        compute_query_id = "on";
                        "pg_stat_statements.max" = 10000;
                        "pg_stat_statements.track" = "all";
                      };
                      initialDatabases = [ { name = "mmo"; } ];
                      port = 5432;
                      listen_addresses = "127.0.0.1";
                      initialScript = ''
                        CREATE EXTENSION IF NOT EXISTS pg_stat_statements;
                        CREATE USER admin SUPERUSER;
                        ALTER USER admin PASSWORD 'admin';
                        GRANT ALL PRIVILEGES ON DATABASE mmo to admin;
                      '';
                    };
                  }
                )
              ];
            };
          };

        # nix fmt
        formatter = pkgs.nixfmt-rfc-style;
      }
    );
}
