-- | The main command line interface
--
-- @since 0.1.0
module Main ( main ) where

import           Control.Monad       ( foldM )

import           Log                 ( initLogger, notice )

import           Options.Applicative
                 ( (<**>), Parser, ParserInfo, ParserPrefs(..), customExecParser
                 , flag', footer, fullDesc, header, help, helper, info
                 , infoOption, long, many, short )

import           ParserResult
                 ( amount, initParserStep, parseStepIO, removeFromDisk, toDisk )

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
    info (arguments <**> version <**> helper)
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
    notice "Welcome to performabot! Processing input from stdin..."
    hSetBuffering stdout LineBuffering
    input <- getContents
    r <- foldM parseStepIO initParserStep $ lines input
    notice . printf "Processing done, found %d results" $ amount r
    nr <- toDisk r
    removeFromDisk nr
