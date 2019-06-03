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
    hlint
    nix-prefetch-git
  ];
}
