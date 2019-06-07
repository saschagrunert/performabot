-- | The main command line interface
--
-- @since 0.1.0
module Main ( main ) where

import           Control.Monad       ( foldM )

import           GoParser            ( parse )

import           Log
                 ( debug, info, initLogger, notice, noticeR )

import qualified Options.Applicative as O ( info )
import           Options.Applicative
                 ( (<**>), Parser, ParserInfo, ParserPrefs(..), customExecParser
                 , flag', footer, fullDesc, header, help, helper, infoOption
                 , long, many, short )

import           Parser              ( State(Ok, Failure, Init) )

import           System.IO
                 ( BufferMode(LineBuffering), hSetBuffering, stdout )
import           System.Log.Logger   ( Priority(..) )

import           Text.Printf         ( printf )

newtype Args = Args Priority

-- | The main function
main :: IO ()
main = customExecParser p parser >>= run
  where
    p = ParserPrefs { prefMultiSuffix     = ""
                    , prefDisambiguate    = True
                    , prefShowHelpOnError = False
                    , prefShowHelpOnEmpty = True
                    , prefBacktrack       = True
                    , prefColumns         = 80
                    }

-- | The main argument parser
parser :: ParserInfo Args
parser =
    O.info (arguments <**> version <**> helper)
           (fullDesc <> header "performabot - Continuous performance analysis reports for software projects"
            <> footer "More info at <https://github.com/saschagrunert/performabot>")

arguments :: Parser Args
arguments = Args <$> verbosity

verbosity :: Parser Priority
verbosity = priority . length
    <$> many (flag' ()
                    (long "verbose" <> short 'v'
                     <> help ("the logging verbosity,"
                              ++ " can be specified up to 2x")))
  where
    priority a
        | a == 0 = NOTICE
        | a == 1 = INFO
        | otherwise = DEBUG

version :: Parser (a -> a)
version =
    infoOption "v0.1.0" (long "version" <> help "Print the current version")

-- | The entry function after argument parsing
run :: Args -> IO ()
run (Args v) = do
    initLogger v
    notice "Welcome to performabot! Processing input from stdin…"
    hSetBuffering stdout LineBuffering
    input <- getContents
    (_, i) <- foldM parseAndPrint (Init, 0) $ lines input
    notice . printf "Processing done, found %d result%s" i $
        if i /= 1 then ("s" :: String) else ""

parseAndPrint :: (State, Integer) -> String -> IO (State, Integer)
parseAndPrint (s, i) line = do
    noticeR line
    let z = parse s line
    debugState z
    return (z, i + res z)

debugState :: State -> IO ()
debugState (Failure f) = info $ printf "Parse error: %s" f
debugState x = debug . printf "State: %s" $ show x

res :: State -> Integer
res (Ok _) = 1
res _ = 0
