let
  pkgs = (import ./nixpkgs.nix { }).pkgsMusl;
in
  (pkgs.haskellPackages.callPackage ./default.nix { }).overrideAttrs(old: {
    doCheck = false;
    doHaddock = false;
    appendConfigureFlags = ["-f static"];
    enableSharedExecutables = false;
    enableSharedLibraries = true;
    configureFlags = [
      "-f static"
      "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
      "--extra-lib-dirs=${pkgs.sqlite.overrideAttrs (old: {
        configureFlags = old.configureFlags
          ++ [ "--disable-shared" "--enable-static" ];
        postInstall = old.postInstall + ''
          mkdir -p $bin/lib
          cp $out/lib/libsqlite3.a $bin/lib
        '';
      })}/lib"
      "--extra-lib-dirs=${pkgs.zlib.static}/lib"
      "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: {
        dontDisableStatic = true;
      })}/lib"
    ];
  })
