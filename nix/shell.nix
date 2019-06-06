let
  pkgs = import ./nixpkgs.nix { };
  project = import ./release.nix;
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  LC_ALL = "C.UTF-8";
  buildInputs = with pkgs; project.env.nativeBuildInputs ++ [
    cabal-install
    cabal2nix
    git
    haskellPackages.hpc-coveralls
    hlint
    nix-prefetch-git
  ];
}
