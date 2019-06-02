let
  nixpkgs = import ./nixpkgs.nix;
  pkgs = import nixpkgs {
    config = { };
  };
  project = import ./release.nix;
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    pkgs.haskellPackages.cabal-install
  ];
}
