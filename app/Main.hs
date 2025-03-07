-- | The main command line interface
--
-- @since 0.1.0
module Main ( main ) where

import           Control.Lens        ( (^.) )
import           Control.Monad       ( foldM )

import           Data.List           ( intercalate )

import           Environment
                 ( Environment(Environment), commit, commitEnvVars
                 , fillEnvironment, owner, ownerEnvVars, pullRequest
                 , pullRequestEnvVars, repository, repositoryEnvVars
                 , tokenEnvVars )

import           Log                 as L ( info )
import           Log                 ( initLogger, notice, warn )

import           Options.Applicative as O ( info )
import           Options.Applicative
                 ( (<**>), Parser, ParserInfo
                 , ParserPrefs(ParserPrefs, prefMultiSuffix, prefDisambiguate,
            prefShowHelpOnEmpty, prefShowHelpOnError, prefBacktrack,
            prefColumns)
                 , customExecParser, flag', footer, fullDesc, header, help
                 , helper, infoOption, long, many, metavar, short, short
                 , strOption, switch, value )

import           Result
                 ( amount, initParserStep, parseStepIO, save )

import           System.Exit         ( exitFailure )
import           System.IO
                 ( BufferMode(LineBuffering), hSetBuffering, stdout )
import           System.Log.Logger   ( Priority(INFO, DEBUG, NOTICE) )

import           Text.Printf         ( printf )

data Args = Args Environment  -- ^ Contains metadata about the current run
                 Priority     -- ^ The log level
                 Bool         -- ^ Development mode without runtime checks

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
arguments = Args <$> environment <*> verbosity <*> local

environment :: Parser Environment
environment = Environment <$> strOption (long "commit" <> short 'c'
                                         <> envHelp "Commit hash" commitEnvVars
                                         <> metavar "COMMIT" <> value "")
    <*> strOption (long "pull-request" <> short 'p'
                   <> envHelp "Pull request number" pullRequestEnvVars
                   <> metavar "PULL_REQUEST" <> value "")
    <*> strOption (long "repository" <> short 'r'
                   <> envHelp "GitHub repository" repositoryEnvVars
                   <> metavar "REPOSITORY" <> value "")
    <*> strOption (long "owner" <> short 'o'
                   <> envHelp "GitHub owner" ownerEnvVars <> metavar "OWNER"
                   <> value "")
    <*> strOption (long "token" <> short 't' <> envHelp "Token" tokenEnvVars
                   <> metavar "TOKEN" <> value "") <**> helper
  where
    envHelp x y = help $ printf "%s - fallback environment  variable%s: $%s"
                                (x :: String)
                                (if length y == 1 then "" else "s" :: String)
                                (intercalate ", $" y)

verbosity :: Parser Priority
verbosity = priority . length
    <$> many (flag' ()
                    (long "verbose" <> short 'v'
                     <> help "Logging verbosity, can be specified up to 2x"))
  where
    priority a
        | a == 0 = NOTICE
        | a == 1 = INFO
        | otherwise = DEBUG

local :: Parser Bool
local = switch (long "local" <> short 'l'
                <> help "Run only locally and do not upload anything")

version :: Parser (a -> a)
version =
    infoOption "v0.1.0" (long "version" <> help "Print the current version")

-- | The entry function after argument parsing
run :: Args -> IO ()
run (Args e v l) = do
    -- Setup logging
    initLogger v
    notice "Welcome to performabot!"
    notice . printf "The logging verbosity is set to: %s" $ show v

    -- Prepare environment
    env <- fillEnvironment e l
    L.info . printf "Using commit: %s" $ env ^. commit
    L.info . printf "Using pull request: %s" $ env ^. pullRequest
    L.info . printf "Using repository: %s" $ env ^. repository
    L.info . printf "Using owner: %s" $ env ^. owner

    -- Parse loop
    notice "Processing input from stdin..."
    hSetBuffering stdout LineBuffering
    input <- getContents
    r <- foldM parseStepIO initParserStep $ lines input

    -- Evaluate and save results if needed
    let c = amount r
    notice . printf "Processing done, found %d result%s" c $
        if c == 1 then "" else "s" :: String
    if c /= 0
        then save l r env
        else do
            warn "Not saving anything because no results found"
            exitFailure
