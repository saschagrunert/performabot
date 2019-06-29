-- | System environment handling
--
-- @since 0.1.0
module Environment
    ( Environment(Environment)
    , commit
    , commitEnvVars
    , fillEnvironment
    , owner
    , ownerEnvVars
    , pullRequest
    , pullRequestEnvVars
    , repository
    , repositoryEnvVars
    , token
    , tokenEnvVars
    ) where

import           Control.Lens       ( (.~), (^.), makeLenses )
import           Control.Monad      ( mapM, msum )

import           Data.List          ( intercalate )

import           Log                ( err )

import           System.Environment ( lookupEnv )
import           System.Exit        ( exitFailure )

import           Text.Printf        ( printf )

data Environment = Environment { _commit      :: String
                               , _pullRequest :: String
                               , _repository  :: String
                               , _owner       :: String
                               , _token       :: String
                               }
    deriving Show

$(makeLenses ''Environment)

-- | Try to fill the environment from local variables
fillEnvironment :: Environment -> Bool -> IO Environment
fillEnvironment e l = do
    c <- getEnv (e ^. commit) "commit" commitEnvVars
    p <- getEnv (e ^. pullRequest) "pull request" pullRequestEnvVars
    r <- getEnv (e ^. repository) "repository" repositoryEnvVars
    o <- getEnv (e ^. owner) "owner" ownerEnvVars
    t <- getEnv (e ^. token) "token" tokenEnvVars

    -- Validate the other environment variables
    if not l && any null [ c, p, r, t ]
        then exitFailure
        else return $ token .~ t $ pullRequest .~ p $ commit .~ c $ owner .~ o $
            repository .~ r $ e

-- | The prefix for local env vars
prefix :: String -> String
prefix = (++) "PB_"

-- | Possible token environment variables sorted by priority
tokenEnvVars :: [String]
tokenEnvVars = [ prefix "TOKEN" ]

-- | Possible repository environment variables sorted by priority
repositoryEnvVars :: [String]
repositoryEnvVars = [ prefix "REPOSITORY", "CIRCLE_PROJECT_REPONAME" ]

-- | Possible repository environment variables sorted by priority
ownerEnvVars :: [String]
ownerEnvVars = [ prefix "OWNER", "CIRCLE_PROJECT_USERNAME" ]

-- | Possible commit environment variables sorted by priority
commitEnvVars :: [String]
commitEnvVars = [ prefix "COMMIT", "CIRCLE_SHA1" ]

-- | Possible pull request environment variables sorted by priority
pullRequestEnvVars :: [String]
pullRequestEnvVars = [ prefix "PULL_REQUEST", "CIRCLE_PR_NUMBER" ]

-- | Generic environment variable retrieval
getEnv :: String -> String -> [String] -> IO String
getEnv "" t v = do
    e <- mapM lookupEnv v
    case msum e of
        Just b -> return b
        _ -> do
            err $ printf ("No %s found via the $%s environment "
                          ++ "variable%s or the command line")
                         t
                         (intercalate ", $" v)
                         (if length v == 1 then "" else "s" :: String)
            return ""

getEnv x _ _ = return x
