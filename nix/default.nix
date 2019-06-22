{ mkDerivation, aeson, ansi-terminal, base, bytestring
, case-insensitive, classy-prelude, classy-prelude-conduit
, classy-prelude-yesod, conduit, containers, data-default
, directory, fast-logger, file-embed, foreign-store
, github-webhooks, hjsmin, hpack, hslogger, http-client
, http-client-tls, http-conduit, lens, megaparsec, monad-control
, monad-logger, optparse-applicative, persistent, persistent-sqlite
, persistent-template, safe, shakespeare, stdenv, tasty
, tasty-hspec, tasty-quickcheck, template-haskell, temporary, text
, time, unordered-containers, uuid, vector, wai, wai-extra
, wai-logger, warp, yaml, yesod, yesod-auth, yesod-core, yesod-form
, yesod-static, yesod-test
}:
mkDerivation {
  pname = "performabot";
  version = "0.1.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-terminal base bytestring case-insensitive classy-prelude
    classy-prelude-conduit classy-prelude-yesod conduit containers
    data-default directory fast-logger file-embed foreign-store
    github-webhooks hjsmin hslogger http-client http-client-tls
    http-conduit lens megaparsec monad-control monad-logger persistent
    persistent-sqlite persistent-template safe shakespeare
    template-haskell temporary text time unordered-containers uuid
    vector wai wai-extra wai-logger warp yaml yesod yesod-auth
    yesod-core yesod-form yesod-static
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base hslogger lens optparse-applicative
  ];
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
