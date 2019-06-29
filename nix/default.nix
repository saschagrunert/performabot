{ mkDerivation, ansi-terminal, base, base64-bytestring, bytestring
, github, hpack, hslogger, lens, megaparsec, optparse-applicative
, parser-combinators, persistent, persistent-sqlite
, persistent-template, stdenv, tasty, tasty-hspec, tasty-quickcheck
, text, time, vector
}:
mkDerivation {
  pname = "performabot";
  version = "0.1.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base base64-bytestring bytestring github hslogger
    lens megaparsec parser-combinators persistent persistent-sqlite
    persistent-template text time vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base hslogger lens optparse-applicative
  ];
  testHaskellDepends = [
    base lens megaparsec tasty tasty-hspec tasty-quickcheck time
  ];
  prePatch = "hpack";
  homepage = "https://github.com/saschagrunert/performabot#readme";
  license = stdenv.lib.licenses.mit;
}
