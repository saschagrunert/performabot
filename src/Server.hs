{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | The server implementation
--
-- @since 0.1.0
module Server ( develMain, getRepl, makeFoundation, makeLogWare, prodMain ) where

import           Control.Lens                         ( (^.) )
import           Control.Monad                        ( when )
import           Control.Monad.Logger
                 ( LogLevel(LevelError), liftLoc, runLoggingT, toLogStr )

import           Data.Default                         ( def )

import           Database.Persist.Sqlite
                 ( createSqlitePool, runMigration, runSqlPool, sqlDatabase
                 , sqlPoolSize )

import           Foundation
                 ( App(..), Route(..), resourcesApp )

import           Handler.Api                          ( getApiR, postApiR )
import           Handler.Common                       ( getFaviconR )
import           Handler.Home                         ( getHomeR, postHomeR )

import           Language.Haskell.TH.Syntax           ( qLocation )

import           Model                                ( migrateAll )

import           Network.HTTP.Client.TLS              ( getGlobalManager )
import           Network.Wai
                 ( Application, Middleware )
import           Network.Wai.Handler.Warp
                 ( Settings, defaultSettings, defaultShouldDisplayException
                 , getPort, runSettings, setHost, setOnException, setPort )
import           Network.Wai.Middleware.RequestLogger
                 ( Destination(Logger), IPAddrSource(FromFallback, FromSocket)
                 , OutputFormat(Apache, Detailed), destination, mkRequestLogger
                 , outputFormat )

import           Settings
                 ( AppSettings, appDatabaseConf, appDetailedRequestLogging
                 , appHost, appIpFromHeader, appMutableStatic, appPort
                 , appStaticDir, configSettingsYmlValue )

import           System.Log.FastLogger
                 ( defaultBufSize, newStdoutLoggerSet )

import           Yesod.Core
                 ( defaultMiddlewaresNoLogging, messageLoggerSource
                 , mkYesodDispatch, toWaiAppPlain )
import           Yesod.Core.Types                     ( loggerSet )
import           Yesod.Default.Config2
                 ( configSettingsYml, develMainHelper, getDevSettings
                 , loadYamlSettings, loadYamlSettingsArgs, makeYesodLogger
                 , useEnv )
import           Yesod.Static                         ( static, staticDevel )

mkYesodDispatch "App" resourcesApp

-- | Main function for production environments
prodMain :: IO ()
prodMain = do
    settings <- loadYamlSettingsArgs [ configSettingsYmlValue ] useEnv
    foundation <- makeFoundation settings
    app <- makeApplication foundation
    runSettings (warpSettings foundation) app

-- | Main function for yesod devel
develMain :: IO ()
develMain = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    devSettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    develMainHelper $ return (devSettings, app)

-- | Retrive the applications REPL
getRepl :: IO (Int, App, Application)
getRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

-- | Retrieve the application settings within a development environment
getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [ configSettingsYml ] [] useEnv

-- | Create the applications foundation
makeFoundation :: AppSettings -> IO App
makeFoundation settings = do
    httpManager <- getGlobalManager
    logger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    stat <- (if settings ^. appMutableStatic then staticDevel else static) $
        settings ^. appStaticDir

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool =
            App settings stat appConnPool httpManager logger
        tempFoundation =
            mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation logger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $
        createSqlitePool (sqlDatabase $ settings ^. appDatabaseConf)
                         (sqlPoolSize $ settings ^. appDatabaseConf)
    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc
    return $ mkFoundation pool

-- | Warp settings for the given foundation value
warpSettings :: App -> Settings
warpSettings foundation = setPort (appSettings foundation ^. appPort) $
    setHost (appSettings foundation ^. appHost) $
    setOnException (\_req e -> when (defaultShouldDisplayException e) $
                    messageLoggerSource foundation
                                        (appLogger foundation)
                                        $(qLocation >>= liftLoc)
                                        "yesod"
                                        LevelError
                                        (toLogStr $
                                         "Exception from Warp: " ++ show e))
                   defaultSettings

-- | Convert our foundation to a WAI Application by calling `toWaiAppPlain` and
-- applying additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    appPlain <- toWaiAppPlain foundation
    return . logWare $ defaultMiddlewaresNoLogging appPlain

-- | Create the logger middleware
makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def { outputFormat = if appSettings foundation
                                             ^. appDetailedRequestLogging
                                         then Detailed True
                                         else Apache (if appSettings foundation
                                                          ^. appIpFromHeader
                                                      then FromFallback
                                                      else FromSocket)
                        , destination  =
                              Logger . loggerSet $ appLogger foundation
                        }
