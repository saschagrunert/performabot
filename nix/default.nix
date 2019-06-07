{ mkDerivation, aeson, base, hpack, hslogger, lens, megaparsec
, optparse-applicative, stdenv, tasty, tasty-hspec
, tasty-quickcheck
}:
mkDerivation {
  pname = "performabot";
  version = "0.1.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ aeson base hslogger lens megaparsec ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base hslogger optparse-applicative ];
  testHaskellDepends = [
    aeson base lens megaparsec tasty tasty-hspec tasty-quickcheck
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/saschagrunert/performabot#readme";
  license = stdenv.lib.licenses.mit;
}
