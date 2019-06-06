{ mkDerivation, base, hpack, lens, megaparsec, stdenv, tasty
, tasty-hspec, tasty-quickcheck
}:
mkDerivation {
  pname = "performabot";
  version = "0.1.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base lens megaparsec ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base lens megaparsec tasty tasty-hspec tasty-quickcheck
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/saschagrunert/performabot#readme";
  license = stdenv.lib.licenses.mit;
}
