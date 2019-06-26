{ mkDerivation, ansi-terminal, base, bytestring, github, hpack
, hslogger, lens, megaparsec, optparse-applicative, persistent
, persistent-sqlite, persistent-template, regex-compat, stdenv
, tasty, tasty-hspec, tasty-quickcheck, text, time, vector
}:
mkDerivation {
  pname = "performabot";
  version = "0.1.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base bytestring github hslogger lens megaparsec
    persistent persistent-sqlite persistent-template regex-compat text
    time vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base hslogger lens optparse-applicative
  ];
  testHaskellDepends = [
    base lens megaparsec tasty tasty-hspec tasty-quickcheck
  ];
  prePatch = "hpack";
  homepage = "https://github.com/saschagrunert/performabot#readme";
  license = stdenv.lib.licenses.mit;
}
