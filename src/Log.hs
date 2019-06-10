-- | The logging faccade
--
-- @since 0.1.0
module Log ( err, debug, debugR, info, infoR, initLogger, notice, noticeR ) where

import           System.Log.Logger ( Priority, debugM, errorM, infoM, noticeM
                                   , setLevel, updateGlobalLogger )

import           Text.Printf       ( printf )

-- | The default logger
logger :: String
logger = "logger"

-- | Logger initialization
initLogger :: Priority -> IO ()
initLogger l = updateGlobalLogger logger (setLevel l)

-- | Output a debug message with prefix
debug :: String -> IO ()
debug m = debugR $ printf "[DEBG]: %s" m

-- | Output a debug message
debugR :: String -> IO ()
debugR = debugM logger

-- | Output an info message with prefix
info :: String -> IO ()
info m = infoR $ printf "[INFO]: %s" m

-- | Output an info message
infoR :: String -> IO ()
infoR = infoM logger

-- | Output an notice message with prefix
notice :: String -> IO ()
notice m = noticeR $ printf "[NOTE]: %s" m

-- | Output an notice message
noticeR :: String -> IO ()
noticeR = noticeM logger

-- | Output an error message with prefix
err :: String -> IO ()
err m = errorM logger $ printf "[ERRO]: %s" m
