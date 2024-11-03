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

        # Erlang
        erlangLatest = pkgs.erlang_27;
        erlangLibs = getErlangLibs erlangLatest;
        erl_app = "lyceum_server";

        # Zig shit (Incomplete)
        zigLatest = pkgs.zig;
        raylib = pkgs.raylib;
        env = zig2nix.outputs.zig-env.${system} {
          #zig = zig2nix.outputs.packages.${system}.zig.master.bin;
          customRuntimeLibs = [
            pkgs.pkg-config
            erlangLibs
            raylib
          ];
          customRuntimeDeps = [
            erlangLibs
            raylib
          ];
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
              beamDeps = builtins.attrValues deps;
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
          client = pkgs.stdenv.mkDerivation {
            pname = "lyceum-client";
            version = "0.0.1";
            src = env.pkgs.lib.cleanSource ./client;

            nativeBuildInputs = [
              pkgs.makeBinaryWrapper
              zigLatest.hook
            ];
            buildInputs = [
              raylib
              erlangLatest
            ];

            postPatch = ''
              ln -s ${pkgs.callPackage ./client/zon-deps.nix { }} $ZIG_GLOBAL_CACHE_DIR/p
            '';
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
          let
            linuxPkgs = with pkgs; [
              inotify-tools
              xorg.libX11
              xorg.libXrandr
              xorg.libXinerama
              xorg.libXcursor
              xorg.libXi
              xorg.libXi
              libGL
            ];
            darwinPkgs = with pkgs.darwin.apple_sdk.frameworks; [
              CoreFoundation
              CoreServices
            ];
          in
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
                      db-up.exec = "just db-up";
                      db-down.exec = "just db-down";
                      db-reset.exec = "just db-reset";
                    };

                    enterShell = ''
                      echo "Starting Development Environment..."
                      just deps
                    '';

                    services.postgres = {
                      package = pkgs.postgresql_16.withPackages (p: with p; [ p.periods ]);
                      enable = true;
                      initialDatabases = [ { name = "mmo"; } ];
                      port = 5432;
                      listen_addresses = "127.0.0.1";
                      initialScript = ''
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
