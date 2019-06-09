{-# LANGUAGE NoImplicitPrelude #-}

-- | Necessary test imports
--
-- @since 0.1.0
module TestImport ( module TestImport, module X ) where

import           ClassyPrelude           as X
                 hiding ( Handler, delete, deleteBy )

import           Control.Lens            ( (^.) )
import           Control.Lens.Setter     ( set )
import           Control.Monad.Logger    ( runLoggingT )

import           Database.Persist        as X hiding ( get )
import           Database.Persist.Sql
                 ( SqlPersistM, connEscapeName, rawExecute, rawSql
                 , runSqlPersistMPool, unSingle )
import           Database.Persist.Sqlite ( createSqlitePoolFromInfo, fkEnabled
                                         , mkSqliteConnectionInfo, sqlDatabase )

import           Foundation              as X

import           Server                  ( makeFoundation, makeLogWare )

import           Settings                ( appDatabaseConf )

import           Test.Tasty.Hspec        as X

import           Yesod.Core              ( messageLoggerSource )
import           Yesod.Core.Unsafe       ( fakeHandlerGetLogger )
import           Yesod.Default.Config2   ( loadYamlSettings, useEnv )
import           Yesod.Test              as X

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings [ "config/test-settings.yml"
                                 , "config/settings.yml"
                                 ]
                                 []
                                 useEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- | This function will truncate all of the tables in the database
wipeDB :: App -> IO ()
wipeDB app = do
    let logFunc = messageLoggerSource app (appLogger app)
        dbName = sqlDatabase $ appSettings app ^. appDatabaseConf
        connInfo = set fkEnabled False $ mkSqliteConnectionInfo dbName

    pool <- runLoggingT (createSqlitePoolFromInfo connInfo 1) logFunc

    flip runSqlPersistMPool pool $ do
        tables <- getTables
        sqlBackend <- ask
        let queries = map (\t -> "DELETE FROM "
                           ++ connEscapeName sqlBackend (DBName t))
                          tables
        forM_ queries (\q -> rawExecute q [])
  where
    getTables = do
        tables
            <- rawSql "SELECT name FROM sqlite_master WHERE type = 'table';" []
        return (fmap unSingle tables)

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    pool <- fmap appConnPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

runHandler :: Handler a -> YesodExample App a
runHandler handler = getTestYesod
    >>= (\app -> fakeHandlerGetLogger appLogger app handler)
