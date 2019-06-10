-- | The main command line interface
--
-- @since 0.1.0
module Main ( main ) where

import           Control.Lens        ( (^.) )
import           Control.Monad       ( foldM )

import           Environment         ( fillEnvironment )

import           Log                 as L ( info )
import           Log                 ( initLogger, notice )

import           Model
                 ( Environment(Environment), environmentBranch
                 , environmentCommit, environmentPullRequest )

import           Options.Applicative as O ( info )
import           Options.Applicative
                 ( (<**>), Parser, ParserInfo, ParserPrefs(..), customExecParser
                 , flag', footer, fullDesc, header, help, helper, infoOption
                 , long, many, metavar, short, short, showDefault, strOption
                 , value )

import           ParserResult
                 ( amount, initParserStep, parseStepIO, removeFromDisk, toDisk )

import           System.IO
                 ( BufferMode(LineBuffering), hSetBuffering, stdout )
import           System.Log.Logger   ( Priority(..) )

import           Text.Printf         ( printf )

data Args = Args Environment Priority

-- | The main function
main :: IO ()
main = customExecParser p parser >>= run
  where
    p = ParserPrefs { prefMultiSuffix     = ""
                    , prefDisambiguate    = True
                    , prefShowHelpOnError = False
                    , prefShowHelpOnEmpty = False
                    , prefBacktrack       = True
                    , prefColumns         = 80
                    }

-- | The main argument parser
parser :: ParserInfo Args
parser =
    O.info (arguments <**> version <**> helper)
           (fullDesc <> header ("performabot - Continuous "
                                ++ "performance analysis reports for "
                                ++ "software projects")
            <> footer ("More info at "
                       ++ "<https://github.com/saschagrunert/performabot>"))

arguments :: Parser Args
arguments = Args <$> environment <*> verbosity

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

environment :: Parser Environment
environment = Environment
    <$> strOption (long "branch" <> short 'b' <> help "Branch name"
                   <> metavar "BRANCH" <> value "")
    <*> strOption (long "commit" <> short 'c' <> help "Commit hash"
                   <> metavar "COMMIT" <> showDefault <> value "")
    <*> strOption (long "pull-request" <> short 'p'
                   <> help "Pull request number" <> metavar "PULL_REQUEST"
                   <> showDefault <> value "")
    <*> strOption (long "token" <> short 't' <> help "Token to be used"
                   <> metavar "TOKEN" <> showDefault <> value "") <**> helper

version :: Parser (a -> a)
version =
    infoOption "v0.1.0" (long "version" <> help "Print the current version")

-- | The entry function after argument parsing
run :: Args -> IO ()
run (Args e v) = do
    initLogger v
    notice "Welcome to performabot!"
    notice . printf "The logging verbosity is set to: %s" $ show v

    env <- fillEnvironment e
    L.info . printf "Using branch: %s" $ env ^. environmentBranch
    L.info . printf "Using commit: %s" $ env ^. environmentCommit
    L.info . printf "Using pull request: %s" $ env ^. environmentPullRequest

    notice "Processing input from stdin..."
    hSetBuffering stdout LineBuffering
    input <- getContents
    r <- foldM parseStepIO initParserStep $ lines input
    notice . printf "Processing done, found %d results" $ amount r
    nr <- toDisk r
    removeFromDisk nr
