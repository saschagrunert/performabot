let
  pkgs = import ./nixpkgs.nix {
    config.allowBroken = true;
    config.packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: {
          floskell = pkgs.haskell.lib.dontCheck super.floskell;
          monad-dijkstra = pkgs.haskell.lib.dontCheck super.monad-dijkstra;
        };
      };
    };
  };
  project = import ./release.nix;
in
pkgs.stdenv.mkDerivation {
  buildInputs = with pkgs; project.env.nativeBuildInputs ++ [
    bash
    cabal-install
    cabal2nix
    conmon
    expect
    file
    git
    glibcLocales
    haskellPackages.floskell
    haskellPackages.hpc-coveralls
    haskellPackages.yesod-bin
    hlint
    iptables
    nix-prefetch-git
    podman
    runc
    sass
    sqlite
    utillinux
    wget
    zlib
  ];
  LANG = "en_US.UTF-8";
  name = "shell";
}
