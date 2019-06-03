{ mkDerivation, base, heavy-logger, mtl, stdenv, text-format-heavy
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
  testHaskellDepends = [ base ];
  license = stdenv.lib.licenses.mit;
}
