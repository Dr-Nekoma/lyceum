{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, devenv, ... } @ inputs:
    let
      systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems = f: builtins.listToAttrs (map (name: { inherit name; value = f name; }) systems);
    in
      {
        packages = forAllSystems (system:
          let
            pkgs = nixpkgs.legacyPackages."${system}";
          in {
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
          poly = "${pkgs.polyml}/bin/polyc";
          mktemp = "${pkgs.coreutils}/bin/mktemp";
        in {
          # test = {
            # type = "app";
            # program = toString (pkgs.writeShellScript "run-tests" ''
              # output=$(${mktemp})
              # ${mlton} -output $output tests/tests.mlb && $output
            # '');
          # };

          build = {
            type = "app";
            program = toString (pkgs.writeShellScript "build-program" ''
              output=$(${mktemp})
              ${poly} -o $output build.sml && echo "Successfully built!"
            '');
          };

          execute = {
            type = "app";
            program = toString (pkgs.writeShellScript "execute-program" ''
              output=$(${mktemp})
              ${poly} -o $output build.sml && $output
            '');
          };
        });

      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          erlangLatest = pkgs.erlang_26;
          zigLatest = pkgs.zig_0_12;
          linuxPkgs = with pkgs; [ inotify-tools ];
          darwinPkgs = with pkgs.darwin.apple_sdk.frameworks; [
            CoreFoundation
            CoreServices
          ];
        in
        {
          # `nix develop .#ci`
          # reduce the number of packages to the bare minimum needed for CI
          ci = pkgs.mkShell {
            buildInputs = with pkgs; [ erlangLatest gnumake rebar3 ];
          };

          # `nix develop`
          default = devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = [
              ({ pkgs, lib, ... }: {
                packages = with pkgs; [
                  erlang-ls
                  erlfmt
                  gnumake
                  rebar3
                ] ++ lib.optionals stdenv.isLinux (linuxPkgs) ++ lib.optionals stdenv.isDarwin darwinPkgs;

                languages.erlang = {
                  enable = true;
                  package = erlangLatest;
                };

                languages.zig = {
                  enable = true;
                  package = zigLatest;
                };

                env = {
                  LOCALE_ARCHIVE = lib.optionalString pkgs.stdenv.isLinux "${pkgs.glibcLocales}/lib/locale/locale-archive";
                  LANG = "en_US.UTF-8";
                  # https://www.erlang.org/doc/man/kernel_app.html
                  ERL_AFLAGS = "-kernel shell_history enabled";
                  ERL_INCLUDE_PATH = "${erlangLatest}/lib/erlang/usr/include";
                };

                enterShell = ''
                  echo "Starting Erlang environment..."
                  rebar3 get-deps
                  rebar3 shell
                '';

                services.postgres = {
                  package = pkgs.postgresql_15.withPackages (p: with p; []);
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
