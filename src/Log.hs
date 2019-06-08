-- | The logging faccade
--
-- @since 0.1.0
module Log ( debug, debugR, info, infoR, initLogger, notice, noticeR ) where

import           System.Log.Logger ( Priority, debugM, infoM, noticeM, setLevel
                                   , updateGlobalLogger )

-- | The default logger
logger :: String
logger = "logger"

-- | The prefix string for non-raw log output
prefix :: String
prefix = "> "

-- | Logger initialization
initLogger :: Priority -> IO ()
initLogger l = updateGlobalLogger logger (setLevel l)

-- | Output a debug message with prefix
debug :: String -> IO ()
debug m = debugR $ prefix ++ m

-- | Output a debug message
debugR :: String -> IO ()
debugR = debugM logger

-- | Output an info message with prefix
info :: String -> IO ()
info m = infoR $ prefix ++ m

-- | Output an info message
infoR :: String -> IO ()
infoR = infoM logger

-- | Output an notice message with prefix
notice :: String -> IO ()
notice m = noticeR $ prefix ++ m

-- | Output an notice message
noticeR :: String -> IO ()
noticeR = noticeM logger
