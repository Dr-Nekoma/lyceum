{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devenv.url = "github:cachix/devenv/v0.6.3";
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
        in
        {
          default = devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = [
              ({ pkgs, lib, ... }: {
                packages = [
                  pkgs.rebar3
                  pkgs.erlang
                  pkgs.erlang-ls
                ];

                services.postgres = {
                  package = pkgs.postgresql_15.withPackages (p: [ ]);
                  enable = true;
                  initialDatabases = [ { name = "crud"; } ];
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
