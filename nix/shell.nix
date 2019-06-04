let
  pkgs = import ./nixpkgs.nix { };
  project = import ./release.nix;
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = with pkgs; project.env.nativeBuildInputs ++ [
    cabal-install
    cabal2nix
    git
    haskellPackages.hpc-coveralls
    hlint
    nix-prefetch-git
  ];
}
