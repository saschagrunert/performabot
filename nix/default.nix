{ mkDerivation, aeson, ansi-terminal, base, bytestring, directory
, hpack, hslogger, http-conduit, lens, megaparsec
, optparse-applicative, persistent, persistent-sqlite
, persistent-template, split, stdenv, tasty, tasty-hspec
, tasty-quickcheck, temporary, text, time
}:
mkDerivation {
  pname = "performabot";
  version = "0.1.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-terminal base bytestring directory hslogger http-conduit
    lens megaparsec persistent persistent-sqlite persistent-template
    split temporary text time
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base hslogger lens optparse-applicative
  ];
  testHaskellDepends = [
    aeson base lens megaparsec tasty tasty-hspec tasty-quickcheck
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/saschagrunert/performabot#readme";
  license = stdenv.lib.licenses.mit;
}
