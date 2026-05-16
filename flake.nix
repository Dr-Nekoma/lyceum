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
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.devenv.flakeModule
        inputs.treefmt-nix.flakeModule
        ./client/flake-module.nix
        ./server/flake-module.nix
      ];

      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      perSystem =
        {
          pkgs,
          system,
          lib,
          ...
        }:
        {
          # This sets `pkgs` to a nixpkgs with allowUnfree option set.
          _module.args.pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
          };

          # Shared constants/versions consumed by client/ and server/
          # via the `lyceum` module argument.
          _module.args.lyceum = {
            app_name = "lyceum";
            app_version = "0.2.3";
            erlangVersion = pkgs.erlang;
            zigVersion = pkgs.zig_0_15;
          };

          # nix build (devenv stuff)
          packages = {
            devenv-up = self.devShells.${system}.default.config.procfileScript;
            devenv-test = self.devShells.${system}.default.config.test;
          };

          # Bare minimum config needed for CI.
          # nix develop .#ci
          devShells.ci = pkgs.mkShell {
            buildInputs = with pkgs; [
              erlang
              just
              rebar3
              rsync
              zig_0_15
              raylib
            ];
          };

          # Shared dev-shell wiring; client/ and server/ flake-modules
          # extend this with their own languages, services, packages,
          # env, and scripts.
          # `nix develop --impure`
          devenv.shells.default = {
            packages = with pkgs; [
              just
            ];

            env = {
              LOCALE_ARCHIVE = lib.optionalString pkgs.stdenv.isLinux "${pkgs.glibcLocales}/lib/locale/locale-archive";
              LANG = "en_US.UTF-8";
              PRE_COMMIT_ALLOW_NO_CONFIG = 1;
            };

            scripts.build.exec = "just build";

            enterShell = ''
              echo "Starting Development Environment..."
            '';

            enterTest = ''
              just test
            '';
          };

          # nix fmt + nix flake check (auto-wired by treefmt-nix flakeModule)
          treefmt = {
            projectRootFile = "flake.nix";
            programs.erlfmt.enable = true;
            programs.nixfmt.enable = true;
            programs.zig.enable = true;
          };
        };

      flake = { };
    };
}
