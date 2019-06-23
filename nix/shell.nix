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
    file
    git
    glibcLocales
    haskellPackages.floskell
    haskellPackages.hpc-coveralls
    hlint
    iptables
    nix-prefetch-git
    podman
    runc
    sqlite
    utillinux
    wget
    zlib
  ];
  LANG = "en_US.UTF-8";
  name = "shell";
}
