{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
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
      zig2nix,
      ...
    }@inputs:
    let
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forAllSystems =
        f:
        builtins.listToAttrs (
          map (name: {
            inherit name;
            value = f name;
          }) systems
        );
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

      mkEnvVars = pkgs: erlangLatest: erlangLibs: raylib: {
        LOCALE_ARCHIVE = pkgs.lib.optionalString pkgs.stdenv.isLinux "${pkgs.glibcLocales}/lib/locale/locale-archive";
        LANG = "en_US.UTF-8";
        # https://www.erlang.org/doc/man/kernel_app.html
        ERL_AFLAGS = "-kernel shell_history enabled";
        ERL_INCLUDE_PATH = "${erlangLatest}/lib/erlang/usr/include";
        ERLANG_INTERFACE_PATH = "${erlangLibs.interface.path}";
        ERLANG_PATH = "${erlangLatest}";
        RAYLIB_PATH = "${raylib}";
        # Devenv sets this to something else
        # https://www.postgresql.org/docs/7.0/libpq-envars.htm
        PGHOST = "127.0.0.1";
      };
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages."${system}";

          # Erlang
          erlangLatest = pkgs.erlang_27;
          erlangLibs = getErlangLibs erlangLatest;

          # Zig shit (Incomplete)
          zigLatest = pkgs.zig;
          raylib = pkgs.raylib;
          env = zig2nix.outputs.zig-env.${system} {
            #zig = zig2nix.outputs.packages.${system}.zig.master.bin;
            customRuntimeLibs = [ pkgs.pkg-config erlangLibs raylib ];
            customRuntimeDeps = [ erlangLibs raylib ];
          };
          system-triple = env.lib.zigTripleFromString system;
        in
        rec {
          devenv-up = self.devShells.${system}.default.config.procfileScript;

          # Leverages nix to build the erlang backend release
          # nix build .#server
          server =
            let
              deps = import ./rebar-deps.nix { inherit (pkgs) fetchHex fetchFromGitHub fetchgit; };
            in
            pkgs.stdenv.mkDerivation {
              name = "server";
              version = "0.0.1";
              src = pkgs.lib.cleanSource ./.;
              buildInputs = with pkgs; [
                erlangLatest
                pkgs.stdenv.cc.cc.lib
                rebar3
                just
                gnutar
              ];
              nativeBuildInputs = with pkgs; [
                autoPatchelfHook
                libz
                ncurses
                openssl
                systemdLibs
              ];
              buildPhase = ''
                mkdir -p _checkouts
                # https://github.com/NixOS/nix/issues/670#issuecomment-1211700127
                export HOME=$(pwd)
                ${toString (
                  pkgs.lib.mapAttrsToList (k: v: ''
                    cp -R --no-preserve=mode ${v} _checkouts/${k}
                  '') deps
                )}
                just release-nix
              '';
              installPhase = ''
                mkdir -p $out
                mkdir -p $out/database
                # Add migrations to the output as well, otherwise the server
                # breaks at runtime.
                cp -r database/migrations $out/database
                tar -xzf _build/prod/rel/*/*.tar.gz -C $out/
              '';
            };

          # nix build .#dockerImage
          dockerImage = pkgs.dockerTools.buildLayeredImage {
            name = "lyceum";
            tag = "latest";
            created = "now";
            # This will copy the compiled erlang release to the image
            contents = [ server pkgs.coreutils pkgs.gawk pkgs.gnugrep ];
            config = {
              Cmd = [
                "${server}/bin/server"
                "foreground"
              ];
              Env = [
                "ERL_DIST_PORT=8001"
                "HOME=/tmp"
              ];
              ExposedPorts = {
                "8080/tcp" = { };
              };
            };
            maxLayers = 30;
          };

          # TODO: Finish this, it's incomplete
          # Leverages nix to build the zig-based client
          # nix build .#client
          client = env.package {
            src = env.pkgs.lib.cleanSource ./client;
            # Prefer nix friendly settings.
            zigPreferMusl = false;
            zigDisableWrap = false;
          };

          # docs = pkgs.stdenv.mkDerivation {
          #   name = "docs";
          #   src = ./.;

          #   installPhase = ''
          #     mkdir -p $out

          #     # remove first heading
          #     # sed -i '1d' README.md

          #     # add frontmatter to markdown file, required by hugo
          #     # sed -i '1s/^/---\ntitle: Railroad\n---\n\n/' README.md

          #     # cp README.md $out/Railroad.md
          #     # cp -r docs $out/
          #   '';
          # };
        }
      );

      apps = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          # Erlang shit
          erlangLatest = pkgs.erlang_27;
          erlangLibs = getErlangLibs erlangLatest;

          # Zig shit
          env = zig2nix.outputs.zig-env.${system} { };
          system-triple = env.lib.zigTripleFromString system;
          raylib = pkgs.raylib;
          zigLatest = pkgs.zig;
        in
        {
          packages.default = env.lib.packages.target.${system-triple}.override {
            # Prefer nix friendly settings.
            zigPreferMusl = false;
            zigDisableWrap = false;
          };

          # nix run .#build
          apps.build =
            env.app [ ]
              "zig build --search-prefix ${erlangLatest} --search-prefix ${raylib} \"$@\"";

          # nix run .#test
          apps.test =
            env.app [ ]
              "zig build --search-prefix ${erlangLatest} --search-prefix ${raylib} test -- \"$@\"";
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          # Erlang shit
          erlangLatest = pkgs.erlang_27;
          erlangLibs = getErlangLibs erlangLatest;

          # Zig shit
          raylib = pkgs.raylib;
          zigLatest = pkgs.zig;

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
              heroku
              just
              rebar3
              zigLatest
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
                      erlang-ls
                      erlfmt
                      just
                      rebar3
                      dbeaver-bin
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
        }
      );
    };
}
