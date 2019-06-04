{ mkDerivation, base, heavy-logger, mtl, stdenv, tasty, tasty-hspec
, tasty-quickcheck, text-format-heavy
}:
mkDerivation {
  pname = "performabot";
  version = "0.1.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [
    base heavy-logger mtl text-format-heavy
  ];
  testHaskellDepends = [ base tasty tasty-hspec tasty-quickcheck ];
  license = stdenv.lib.licenses.mit;
}
