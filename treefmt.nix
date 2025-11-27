# treefmt.nix
{ pkgs, ... }:
{
  # Used to find the project root
  projectRootFile = "flake.nix";
  programs = {
    nixfmt.enable = true;
    sqlfluff = {
      enable = true;
      dialect = "postgres";
    };
  };
}
