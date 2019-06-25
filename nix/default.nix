{ mkDerivation, aeson, ansi-terminal, base, hpack, hslogger, lens
, megaparsec, optparse-applicative, persistent, persistent-sqlite
, persistent-template, regex-compat, split, stdenv, tasty
, tasty-hspec, tasty-quickcheck, text, time
}:
mkDerivation {
  pname = "performabot";
  version = "0.1.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-terminal base hslogger lens megaparsec persistent
    persistent-sqlite persistent-template regex-compat split text time
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base hslogger lens optparse-applicative
  ];
  testHaskellDepends = [
    aeson base lens megaparsec tasty tasty-hspec tasty-quickcheck
  ];
  prePatch = "hpack";
  homepage = "https://github.com/saschagrunert/performabot#readme";
  license = stdenv.lib.licenses.mit;
}
