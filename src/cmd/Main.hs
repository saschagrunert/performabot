-- | The main command line interface
--
-- @since 0.1.0
module Main ( main ) where

import           Control.Monad ( foldM_ )

import           GoParser      ( parse )

import           Parser        ( State(Init) )

import           System.IO
                 ( BufferMode(LineBuffering), hSetBuffering, stdout )

-- | The main function
main :: IO ()
main = do
    putStrLn "Welcome to performabot! Waiting for input from stdin…"
    hSetBuffering stdout LineBuffering
    input <- getContents
    foldM_ parseAndPrint Init (lines input)

-- | Prints the current line to the logging faccade and runs the parser on it
parseAndPrint :: State -> String -> IO State
parseAndPrint s line = do
    putStrLn line
    let ns = parse s line
    print ns
    return ns
