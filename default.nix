{ mkDerivation, base, heavy-logger, mtl, stdenv, text-format-heavy
}:
mkDerivation {
  pname = "performabot";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base heavy-logger mtl text-format-heavy
  ];
  license = stdenv.lib.licenses.bsd3;
}
