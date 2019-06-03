{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main ( main ) where

import           Control.Monad.Trans        ( liftIO )

import           Data.Text.Format.Heavy     ( Single(Single) )

import           Lib                        ( parse )

import           System.IO
                 ( BufferMode(LineBuffering), hSetBuffering, stdout )
import           System.Log.Heavy
                 ( LogContextFrame(LogContextFrame)
                 , LoggingSettings(LoggingSettings), LoggingT, debug_level
                 , defStdoutSettings, include, lsFormat, withLogContext
                 , withLoggingT )
import           System.Log.Heavy.Shortcuts ( debug, info )

-- | The main function
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    defaultLogger $ do
        info "Welcome to performabot! Waiting for input from stdin…" ()
        input <- liftIO getContents
        liftIO . mapM_ parseAndPrint $ lines input

-- | The default logger settings
defaultLogger :: LoggingT IO a -> IO a
defaultLogger = withLoggingT settings
    . withLogContext (LogContextFrame [] (include [ ([], debug_level) ]))
  where
    settings = LoggingSettings $ defStdoutSettings { lsFormat = logFormat }

    logFormat = "[{level}]: {message}\n"

-- | Prints the current line to the logging faccade and runs the parser on it
parseAndPrint :: String -> IO ()
parseAndPrint line = defaultLogger $ do
    info "{}" $ Single line
    let succeed = parse line
    debug "Parsing {}" . Single $
        if succeed then ("succeed" :: String) else ("failed" :: String)
