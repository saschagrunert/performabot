let
  nixpkgs = import ./nixpkgs.nix;
  pkgs = import nixpkgs {
    config = { };
  };
in
  pkgs.haskellPackages.callPackage ./default.nix { }
