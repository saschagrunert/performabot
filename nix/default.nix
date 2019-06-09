{ mkDerivation, aeson, base, bytestring, case-insensitive
, classy-prelude, classy-prelude-conduit, classy-prelude-yesod
, conduit, containers, data-default, directory, fast-logger
, file-embed, foreign-store, hjsmin, hpack, hslogger
, http-client-tls, http-conduit, lens, megaparsec, monad-control
, monad-logger, optparse-applicative, persistent, persistent-sqlite
, persistent-template, safe, shakespeare, stdenv, tasty
, tasty-hspec, tasty-quickcheck, template-haskell, temporary, text
, time, unordered-containers, vector, wai, wai-extra, wai-logger
, warp, yaml, yesod, yesod-auth, yesod-core, yesod-form
, yesod-static, yesod-test
}:
mkDerivation {
  pname = "performabot";
  version = "0.1.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring case-insensitive classy-prelude
    classy-prelude-conduit classy-prelude-yesod conduit containers
    data-default directory fast-logger file-embed foreign-store hjsmin
    hslogger http-client-tls http-conduit lens megaparsec monad-control
    monad-logger persistent persistent-sqlite persistent-template safe
    shakespeare template-haskell temporary text time
    unordered-containers vector wai wai-extra wai-logger warp yaml
    yesod yesod-auth yesod-core yesod-form yesod-static
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base hslogger optparse-applicative ];
  testHaskellDepends = [
    aeson base classy-prelude directory lens megaparsec monad-logger
    persistent persistent-sqlite tasty tasty-hspec tasty-quickcheck
    yesod yesod-core yesod-test
  ];
  doHaddock = false;
  preConfigure = "hpack";
  homepage = "https://github.com/saschagrunert/performabot#readme";
  license = stdenv.lib.licenses.mit;
}
