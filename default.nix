{ sources ? import ./nix/sources.nix
}:
let pkgs = import sources.nixpkgs {};
in
{ coloring = pkgs.haskellPackages.callPackage ./coloring.nix { };
}
