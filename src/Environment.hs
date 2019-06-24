-- | System environment handling
--
-- @since 0.1.0
module Environment
    ( Environment(Environment)
    , commit
    , commitEnvVars
    , fillEnvironment
    , pullRequest
    , pullRequestEnvVars
    , repoSlug
    , repoSlugEnvVars
    , tokenEnvVars
    ) where

import           Control.Lens       ( (.~), (^.), makeLenses )
import           Control.Monad      ( mapM, msum )

import           Data.Aeson.TH
                 ( defaultOptions, deriveJSON, fieldLabelModifier )
import           Data.List          ( intercalate )
import           Data.List.Split    ( splitOn )

import           Log                ( err )

import           System.Environment ( lookupEnv )
import           System.Exit        ( exitFailure )

import           Text.Printf        ( printf )

data Environment = Environment { _commit      :: String
                               , _pullRequest :: String
                               , _repoSlug    :: String
                               , _token       :: String
                               }
    deriving Show

$(makeLenses ''Environment)

deriveJSON defaultOptions { fieldLabelModifier = drop 1 } ''Environment

fillEnvironment :: Environment -> Bool -> IO Environment
fillEnvironment e d = do
    c <- getEnv (e ^. commit) "commit" commitEnvVars
    p <- getEnv (e ^. pullRequest) "pull request" pullRequestEnvVars
    r <- getEnv (e ^. repoSlug) "repo slug" repoSlugEnvVars
    t <- getEnv (e ^. token) "token" tokenEnvVars

    -- Validate the other environment variables
    if not d && any null [ c, p, r, t ]
        then exitFailure
        else return $ token .~ t $ pullRequest .~ p $ commit .~ c $
            repoSlug .~ r $ e

-- | The prefix for local env vars
prefix :: String -> String
prefix = (++) "PB_"

-- | The split chars for defining multiple env vars in one single value
envVarSplit :: String
envVarSplit = "/$"

-- | Possible token environment variables sorted by priority
tokenEnvVars :: [String]
tokenEnvVars = [ prefix "TOKEN" ]

-- | Possible repository slug (`username/project`) environment variables sorted
-- by priority
repoSlugEnvVars :: [String]
repoSlugEnvVars =
    [ prefix "REPOSLUG"
    , "CIRCLE_PROJECT_USERNAME" ++ envVarSplit ++ "CIRCLE_PROJECT_REPONAME"
    , "TRAVIS_REPO_SLUG"
    ]

-- | Possible commit environment variables sorted by priority
commitEnvVars :: [String]
commitEnvVars = [ prefix "COMMIT", "CIRCLE_SHA1", "TRAVIS_COMMIT" ]

-- | Possible pull request environment variables sorted by priority
pullRequestEnvVars :: [String]
pullRequestEnvVars =
    [ prefix "PULL_REQUEST", "CIRCLE_PR_NUMBER", "TRAVIS_PULL_REQUEST" ]

-- | Generic environment variable retrieval
getEnv :: String -> String -> [String] -> IO String
getEnv "" t v = do
    e <- mapM lookupSplitEnv v
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

-- | Lookup env vars but split them by `envVarSplit` before
lookupSplitEnv :: String -> IO (Maybe String)
lookupSplitEnv i = do
    e <- mapM lookupEnv $ splitOn envVarSplit i
    return $ intercalate "/" <$> sequence e
