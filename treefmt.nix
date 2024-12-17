# treefmt.nix
{ pkgs, ... }:
{
  # Used to find the project root
  projectRootFile = "flake.nix";
  programs.nixpkgs-fmt.enable = true;
}
