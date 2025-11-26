{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
    };

    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs =
    {
      self,
      nixpkgs,
      devenv,
      flake-parts,
      treefmt-nix,
      ...
    }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      perSystem =
        { pkgs, system, ... }:
        let
          # Environment-specific packages
          linuxPkgs = with pkgs; [
            inotify-tools
            glfw
            libGL
            libpulseaudio
            libxkbcommon
            pkg-config
            xorg.libxcb
            xorg.libXft
            xorg.libX11
            xorg.libX11.dev
            xorg.libXrandr
            xorg.libXinerama
            xorg.libXcursor
            xorg.libXi
            # Wayland stuff
            glfw-wayland
            wayland
            wayland-protocols
            wayland-scanner
          ];
          linuxLibs =
            with pkgs;
            lib.makeLibraryPath [
              libGL
              libxkbcommon
              liburing
              raylib
              xorg.libxcb
              xorg.libXft
              xorg.libX11
              xorg.libX11.dev
              xorg.libXrandr
              xorg.libXinerama
              xorg.libXcursor
              xorg.libXi
              # Wayland stuff
              glfw-wayland
              wayland
              wayland-protocols
            ];
          darwinPkgs = with pkgs; [ libiconv ];

          devPackages =
            with pkgs;
            [
              erlang-language-platform
              erlfmt
              git-lfs
              just
              postgresql
              raylib
              sqls
              watchman
            ]
            ++ lib.optionals stdenv.isLinux linuxPkgs
            ++ lib.optionals stdenv.isDarwin darwinPkgs;

          # App config
          app_name = "lyceum";
          app_version = "0.2.3";

          # Erlang
          erlangVersion = pkgs.erlang;
          erl_app = "server";

          # Zig
          zig_app = "lyceum-client";
          zigVersion = pkgs.zig_0_15;
          raylib = pkgs.raylib;

          mkEnvVars = pkgs: erl: raylib: {
            LOCALE_ARCHIVE = pkgs.lib.optionalString pkgs.stdenv.isLinux "${pkgs.glibcLocales}/lib/locale/locale-archive";
            LANG = "en_US.UTF-8";
            # https://www.erlang.org/doc/man/kernel_app.html
            ERL_AFLAGS = "-kernel shell_history enabled";
            ERL_INCLUDE_PATH = "${erl}/lib/erlang/usr/include";
            # Devenv sets this to something else
            # https://www.postgresql.org/docs/7.0/libpq-envars.htm
            PGHOST = "127.0.0.1";
            PRE_COMMIT_ALLOW_NO_CONFIG = 1;
            # Waylad setup
            GLFW_SCALE_TO_MONITOR = "GLFW_TRUE";
          };

          treefmtEval = treefmt-nix.lib.evalModule pkgs ./treefmt.nix;
        in
        {
          # This sets `pkgs` to a nixpkgs with allowUnfree option set.
          _module.args.pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
          };

          # nix build
          packages = rec {
            # devenv up
            devenv-up = self.devShells.${system}.default.config.procfileScript;

            # devenv test
            devenv-test = self.devShells.${system}.default.config.test;

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
                version = app_version;
                root = ./server;
                src = pkgs.lib.cleanSource ./server;
                releaseType = "release";
                profile = "prod";
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

            # nix build .#dockerImage
            dockerImage = pkgs.dockerTools.buildLayeredImage {
              name = erl_app;
              tag = "latest";
              created = "now";
              # This will copy the compiled erlang release to the image
              contents = [
                server
              ];
              config = {
                Volumes = {
                  "/opt/${erl_app}/etc" = { };
                  "/opt/${erl_app}/data" = { };
                  "/opt/${erl_app}/log" = { };
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
                  "4369/tcp" = { };
                  "4369/ucp" = { };
                  "8080/tcp" = { };
                  "8080/udp" = { };
                };
              };
            };

            # nix build .#client
            client = pkgs.stdenv.mkDerivation {
              pname = zig_app;
              version = app_version;
              src = pkgs.lib.cleanSource ./client;

              zigBuildFlags = [
                "-fsys=raylib"
                "--release=fast"
                "-Dassets=${builtins.toString ./client}/assets"
              ];

              nativeBuildInputs = [
                zigVersion.hook
                pkgs.makeWrapper
              ];

              buildInputs =
                with pkgs;
                [
                  raylib
                  zigVersion
                  erlangVersion
                ]
                ++ lib.optionals stdenv.isLinux (linuxPkgs)
                ++ lib.optionals stdenv.isDarwin darwinPkgs;

              # To re-generate the nix lockfile:
              # just client-deps
              postPatch = ''
                ln -s ${pkgs.callPackage ./client/build.zig.zon.nix { }} $ZIG_GLOBAL_CACHE_DIR/p
              '';

              postInstall = ''
                wrapProgram "$out/bin/${zig_app}" --prefix LD_LIBRARY_PATH ":" "${linuxLibs}"
              '';
            };

          };

          # nix run
          apps = {
          };

          devShells = {
            # `nix develop .#ci`
            # reduce the number of packages to the bare minimum needed for CI
            ci = pkgs.mkShell {
              env = mkEnvVars pkgs erlangVersion raylib;
              buildInputs = with pkgs; [
                erlangVersion
                just
                rebar3
                rsync
                zigVersion
                raylib
              ];
            };

            # `nix develop --impure`
            default = devenv.lib.mkShell {
              inherit inputs pkgs;
              modules = [
                (import ./devshell.nix {
                  inherit
                    pkgs
                    mkEnvVars
                    erlangVersion
                    zigVersion
                    raylib
                    app_name
                    system
                    ;
                  packages = devPackages;
                })
              ];
            };
          };

          # nix fmt
          formatter = treefmtEval.config.build.wrapper;
        };

      flake = {
      };
    };
}
