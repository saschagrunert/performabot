let
  pkgs = import ./nixpkgs.nix { overlays = [(import ./overlay.nix)]; };
in
  pkgs.haskellPackages.callPackage ./default.nix { }
