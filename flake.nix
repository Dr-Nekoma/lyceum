{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    zig2nix.url = "github:Cloudef/zig2nix";
  };

  outputs = { self, nixpkgs, devenv, zig2nix, ... } @ inputs:
    let
      systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems = f: builtins.listToAttrs (map (name: { inherit name; value = f name; }) systems);
      getErlangLibs = erlangPkg: 
        let
            erlangPath = "${erlangPkg}/lib/erlang/lib/";
            dirs = builtins.attrNames (builtins.readDir erlangPath);
            interfaceVersion = builtins.head (builtins.filter (s: builtins.substring 0 13 s == "erl_interface") dirs);
            interfacePath = erlangPath + interfaceVersion;
        in
        {
            path = erlangPath;
            dirs = dirs;
            interface = { version = interfaceVersion; path = interfacePath; };
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
      };
    in
      {
        packages = forAllSystems (system:
          let
            pkgs = nixpkgs.legacyPackages."${system}";
            env = zig2nix.outputs.zig-env.${system} {};
            system-triple = env.lib.zigTripleFromString system;
          in {
            devenv-up = self.devShells.${system}.default.config.procfileScript;
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
          });

      apps = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          # Erlang shit
          erlangLatest = pkgs.erlang_26;
          erlangLibs = getErlangLibs erlangLatest;

          # Zig shit
          env = zig2nix.outputs.zig-env.${system} {};
          system-triple = env.lib.zigTripleFromString system;
          raylib = pkgs.raylib;
          zigLatest = pkgs.zig_0_12;
        in {
          packages.default = env.lib.packages.target.${system-triple}.override {
            # Prefer nix friendly settings.
            zigPreferMusl = false;
            zigDisableWrap = false;
          };

          # nix run .#build
          apps.build = env.app [] "zig build --search-prefix ${erlangLatest} --search-prefix ${raylib} \"$@\"";
          
          # nix run .#test
          apps.test = env.app [] "zig build --search-prefix ${erlangLatest} --search-prefix ${raylib} test -- \"$@\"";
        });

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};

          # Erlang shit
          erlangLatest = pkgs.erlang_26;
          erlangLibs = getErlangLibs erlangLatest;

          # Zig shit
          env = zig2nix.outputs.zig-env.${system} {};
          system-triple = env.lib.zigTripleFromString system;
          raylib = pkgs.raylib;
          zigLatest = pkgs.zig_0_12;

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
            buildInputs = with pkgs; [ erlangLatest just rebar3 zigLatest ];
          };

          # `nix develop`
          default = devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = [
              ({ pkgs, lib, ... }: {
                packages = with pkgs; [
                  erlang-ls
                  erlfmt
                  just
                  rebar3
                  dbeaver
                ] ++ lib.optionals stdenv.isLinux (linuxPkgs) ++ lib.optionals stdenv.isDarwin darwinPkgs;

                languages.erlang = {
                  enable = true;
                  package = erlangLatest;
                };

                languages.zig = {
                  enable = true;
                  package = zigLatest;
                };

                env = mkEnvVars pkgs erlangLatest erlangLibs raylib;

                enterShell = ''
                  echo "Starting Erlang environment..."
                  rebar3 get-deps
                '';

                services.postgres = {
                  package = pkgs.postgresql_15.withPackages (p: with p; [p.periods]);
                  enable = true;
                  initialDatabases = [ { name = "mmo"; } ];
                  port = 5432;
                  listen_addresses = "127.0.0.1";
                  initialScript = ''
                  CREATE USER admin SUPERUSER;
                  ALTER USER admin PASSWORD 'admin';
                  '';
                };
              })
            ];
          };
        });
    };
}
