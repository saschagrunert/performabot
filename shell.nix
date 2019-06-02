let
  pkgs = import ./nixpkgs.nix { };
  project = import ./release.nix;
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    pkgs.haskellPackages.cabal-install
  ];
}
