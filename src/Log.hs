-- | The logging faccade
--
-- @since 0.1.0
module Log ( err, debug, debugR, info, infoR, initLogger, notice, noticeR ) where

import           System.Console.ANSI
                 ( Color(Blue, Green, Red, White), ColorIntensity(Vivid)
                 , ConsoleLayer(Foreground), SGR(SetColor, Reset), hSetSGR
                 , setSGR )
import           System.IO           ( stderr )
import           System.Log.Logger   ( Priority, debugM, errorM, infoM, noticeM
                                     , setLevel, updateGlobalLogger )

import           Text.Printf         ( printf )

-- | The default logger
logger :: String
logger = "logger"

-- | Logger initialization
initLogger :: Priority -> IO ()
initLogger l = updateGlobalLogger logger (setLevel l)

-- | Output a debug message with prefix
debug :: String -> IO ()
debug m = do
    hSetSGR stderr [ SetColor Foreground Vivid White ]
    debugR $ printf "[DEBG]: %s" m
    setSGR [ Reset ]

-- | Output a debug message
debugR :: String -> IO ()
debugR = debugM logger

-- | Output an info message with prefix
info :: String -> IO ()
info m = do
    hSetSGR stderr [ SetColor Foreground Vivid Green ]
    infoR $ printf "[INFO]: %s" m
    setSGR [ Reset ]

-- | Output an info message
infoR :: String -> IO ()
infoR = infoM logger

-- | Output an notice message with prefix
notice :: String -> IO ()
notice m = do
    hSetSGR stderr [ SetColor Foreground Vivid Blue ]
    noticeR $ printf "[NOTE]: %s" m
    setSGR [ Reset ]

-- | Output an notice message
noticeR :: String -> IO ()
noticeR = noticeM logger

-- | Output an error message with prefix
err :: String -> IO ()
err m = do
    hSetSGR stderr [ SetColor Foreground Vivid Red ]
    errorM logger $ printf "[ERRO]: %s" m
    setSGR [ Reset ]
