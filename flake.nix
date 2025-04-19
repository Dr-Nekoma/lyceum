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

    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs =
    {
      self,
      nixpkgs,
      devenv,
      flake-utils,
      treefmt-nix,
      ...
    }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
          };
        };

        treefmtEval = treefmt-nix.lib.evalModule pkgs ./treefmt.nix;

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
        ];
        linuxLibs =
          with pkgs;
          lib.makeLibraryPath [
            libGL
            libxkbcommon
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
        darwinPkgs = with pkgs.darwin.apple_sdk.frameworks; [
          CoreFoundation
          CoreServices
        ];

        devPackages = 
          with pkgs;
          [
            just
            raylib
            sqls
          ]
          ++ lib.optionals stdenv.isLinux linuxPkgs
          ++ lib.optionals stdenv.isDarwin darwinPkgs;

        # App config
        app_name = "lyceum";
        app_version = "0.1.0";

        # Erlang
        erlangVersion = pkgs.erlang;
        erlangLibs = getErlangLibs erlangVersion;
        erl_app = "server";

        # Zig
        zig_app = "lyceum-client";
        zigVersion = pkgs.zig;
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
              version = app_version;
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
              with pkgs; [ raylib zigVersion erlangVersion ]
                ++ lib.optionals stdenv.isLinux (linuxPkgs)
                ++ lib.optionals stdenv.isDarwin darwinPkgs;

            postPatch = ''
              ln -s ${pkgs.callPackage ./client/zon-deps.nix { }} $ZIG_GLOBAL_CACHE_DIR/p
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
                inherit pkgs mkEnvVars erlangVersion zigVersion raylib app_name;
                packages = devPackages;
              })
            ];
          };
        };

        # nix fmt
        formatter = treefmtEval.config.build.wrapper;
      }
    );
}
