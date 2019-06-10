-- | The main for the GHCi repl
--
-- @since 0.1.0
module GHCi ( update, shutdown ) where

import           Control.Concurrent
                 ( MVar, ThreadId, forkFinally, killThread, newEmptyMVar
                 , putMVar, takeMVar )
import           Control.Monad            ( (>=>) )

import           Data.IORef
                 ( IORef, newIORef, readIORef, writeIORef )

import           Foreign.Store            ( Store(Store), lookupStore, readStore
                                          , storeAction, withStore )

import           GHC.Word                 ( Word32 )

import           Network.Wai.Handler.Warp
                 ( defaultSettings, runSettings, setPort )

import           Server                   ( getRepl )

update :: IO ()
update = do
    mtidStore <- lookupStore tidStoreNum
    case mtidStore of
          -- no server running
        Nothing -> do
            done <- storeAction doneStore newEmptyMVar
            tid <- start done
            _ <- storeAction (Store tidStoreNum) (newIORef tid)
            return ()
          -- server is already running
        Just tidStore -> restartAppInNewThread tidStore
  where
    doneStore :: Store (MVar ())
    doneStore = Store 0

    restartAppInNewThread :: Store (IORef ThreadId) -> IO ()
    restartAppInNewThread tidStore = modifyStoredIORef tidStore $ \tid -> do
        killThread tid
        withStore doneStore takeMVar
        readStore doneStore >>= start

    start :: MVar () -> IO ThreadId
    start done = do
        (port, site, app) <- getRepl
        forkFinally (runSettings (setPort port defaultSettings) app)
                    (\_ -> putMVar done () >> (\_ -> return ()) site)

shutdown :: IO ()
shutdown = do
    mtidStore <- lookupStore tidStoreNum
    case mtidStore of
        Nothing -> putStrLn "no app running"
        Just tidStore -> do
            withStore tidStore $ readIORef >=> killThread
            putStrLn "app is shutdown"

tidStoreNum :: Word32
tidStoreNum = 1

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f = withStore store $ \ref -> do
    v <- readIORef ref
    f v >>= writeIORef ref
