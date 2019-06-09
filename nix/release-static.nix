let
  pkgs = (import ./nixpkgs.nix { }).pkgsMusl;
in
  (pkgs.haskellPackages.callPackage ./default.nix { }).overrideAttrs(old: {
    enableSharedExecutables = false;
    enableSharedLibraries = true;
    configureFlags = [
      "--ghc-option=-optl=-static"
      "--ghc-option=-fPIC"
      "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
      "--extra-lib-dirs=${pkgs.zlib.static}/lib"
      "--extra-lib-dirs=${pkgs.libffi.overrideAttrs
        (old: { dontDisableStatic = true; })}/lib"
      "--disable-executable-stripping"
    ];
  })