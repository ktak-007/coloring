{ pkgs ? import <nixpkgs> {} }:

pkgs.haskell.lib.buildStackProject {
  name = "coloring";
  src = ./.;
}
