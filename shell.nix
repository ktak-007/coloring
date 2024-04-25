{ nixpkgs ? import <nixpkgs> {}
, sources ? import ./nix/sources.nix
}:
let
  name = "Coloring";

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
    ] ++ (with nixpkgs; [
      figlet
      boxes
      toilet
      lolcat
      onefetch
    ]);

  shellHook = ''
    echo
    echo "${name}" | figlet -t -f banner3-D | lolcat -f
    onefetch
    echo -e "${name} development shell\n\nUsage:\tnix-build\n\tcat test/atf.txt | result/bin/coloring\n\tghcid --command 'cabal repl'" | boxes -d parchment | lolcat -f
    echo
  '';

  }
