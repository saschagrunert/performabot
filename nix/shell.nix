let
  pkgs = import ./nixpkgs.nix { };
  project = import ./release.nix;
in
pkgs.stdenv.mkDerivation {
  buildInputs = with pkgs; project.env.nativeBuildInputs ++ [
    cabal-install
    cabal2nix
    expect
    git
    glibcLocales
    haskellPackages.hpc-coveralls
    haskellPackages.yesod-bin
    hlint
    nix-prefetch-git
    sass
    wget
    zlib
  ];
  LANG = "en_US.UTF-8";
  name = "shell";
}
