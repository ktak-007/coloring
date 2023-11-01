{ nixpkgs ? import <nixpkgs> {}
, sources ? import ./nix/sources.nix
}:
let
  pkgs = import sources.nixpkgs {};

  haskell = pkgs.haskellPackages;
  haskellWithPackages = haskell.ghcWithPackages (hpkgs: [
      hpkgs.pretty-simple
  ]);

in
  nixpkgs.mkShell {
    buildInputs = [
      haskellWithPackages
      haskell.ghcid
      haskell.cabal-install
      haskell.cabal2nix
      haskell.haskell-language-server
    ];
  }
