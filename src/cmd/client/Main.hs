-- | The main command line interface
--
-- @since 0.1.0
module Main ( main ) where

import           Control.Lens        ( (^.) )
import           Control.Monad       ( foldM )

import           Data.List           ( intercalate )

import           Environment
                 ( branchEnvVars, commitEnvVars, fillEnvironment
                 , pullRequestEnvVars, tokenEnvVars )

import           Log                 as L ( info )
import           Log                 ( initLogger, notice, warn )

import           Model
                 ( Environment(Environment), environmentBranch
                 , environmentCommit, environmentPullRequest )

import           Options.Applicative as O ( info )
import           Options.Applicative
                 ( (<**>), Parser, ParserInfo, ParserPrefs(..), customExecParser
                 , flag', footer, fullDesc, header, help, helper, infoOption
                 , internal, long, many, metavar, short, short, showDefault
                 , strOption, switch, value )

import           ParserResult
                 ( amount, initParserStep, parseStepIO, send )

import           System.Exit         ( exitFailure )
import           System.IO
                 ( BufferMode(LineBuffering), hSetBuffering, stdout )
import           System.Log.Logger   ( Priority(..) )

import           Text.Printf         ( printf )

data Args = Args Environment  -- ^ Contains data sent to the server
                 Priority     -- ^ The log level
                 String       -- ^ The API url
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
arguments = Args <$> environment <*> verbosity <*> apiUrl <*> devel

environment :: Parser Environment
environment = Environment <$> strOption (long "branch" <> short 'b'
                                         <> envHelp "Branch name" branchEnvVars
                                         <> metavar "BRANCH" <> value "")
    <*> strOption (long "commit" <> short 'c'
                   <> envHelp "Commit hash" commitEnvVars <> metavar "COMMIT"
                   <> value "")
    <*> strOption (long "pull-request" <> short 'p'
                   <> envHelp "Pull request number" pullRequestEnvVars
                   <> metavar "PULL_REQUEST" <> value "")
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
                     <> help ("the logging verbosity,"
                              ++ " can be specified up to 2x")))
  where
    priority a
        | a == 0 = NOTICE
        | a == 1 = INFO
        | otherwise = DEBUG

apiUrl :: Parser String
apiUrl = strOption (long "url" <> short 'u' <> help "API url for sending data"
                    <> metavar "URL" <> showDefault
                    <> value "http://localhost:3000/api")

devel :: Parser Bool
devel = switch (internal <> long "devel" <> short 'd')

version :: Parser (a -> a)
version =
    infoOption "v0.1.0" (long "version" <> help "Print the current version")

-- | The entry function after argument parsing
run :: Args -> IO ()
run (Args e v u d) = do
    -- Setup logging
    initLogger v
    notice "Welcome to performabot!"
    notice . printf "The logging verbosity is set to: %s" $ show v

    -- Prepare environment
    env <- fillEnvironment e d
    L.info . printf "Using branch: %s" $ env ^. environmentBranch
    L.info . printf "Using commit: %s" $ env ^. environmentCommit
    L.info . printf "Using pull request: %s" $ env ^. environmentPullRequest

    -- Parse loop
    notice "Processing input from stdin..."
    hSetBuffering stdout LineBuffering
    input <- getContents
    r <- foldM parseStepIO initParserStep $ lines input

    -- Evaluate and send results if needed
    let c = amount r
    notice . printf "Processing done, found %d result%s" c $
        if c == 1 then "" else "s" :: String
    if c /= 0
        then send r u env
        else do
            warn "Not sending anything because no results found"
            exitFailure
