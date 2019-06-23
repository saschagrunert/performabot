-- | System environment handling
--
-- @since 0.1.0
module Env
    ( commitEnvVars
    , fillEnvironment
    , pullRequestEnvVars
    , repoSlugEnvVars
    , tokenEnvVars
    ) where

import           Control.Lens       ( (.~), (^.) )
import           Control.Monad      ( mapM, msum )

import           Data.List          ( intercalate )
import           Data.List.Split    ( splitOn )
import           Data.Text          ( Text, pack )
import qualified Data.Text          as T ( null )

import           Log                ( err )

import           Model
                 ( Environment, environmentCommit, environmentPullRequest
                 , environmentRepoSlug, environmentToken )

import           System.Environment ( lookupEnv )
import           System.Exit        ( exitFailure )

import           Text.Printf        ( printf )

fillEnvironment :: Environment -> Bool -> IO Environment
fillEnvironment e d = do
    c <- getEnv (e ^. environmentCommit) "commit" commitEnvVars
    p <- getEnv (e ^. environmentPullRequest) "pull request" pullRequestEnvVars
    r <- getEnv (e ^. environmentRepoSlug) "repo slug" repoSlugEnvVars
    t <- getEnv (e ^. environmentToken) "token" tokenEnvVars

    -- Validate the other environment variables
    if not d && any T.null [ r, c, p, t ]
        then exitFailure
        else return $ environmentToken .~ t $ environmentPullRequest .~ p $
            environmentCommit .~ c $ environmentRepoSlug .~ r $ e

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
getEnv :: Text -> String -> [String] -> IO Text
getEnv "" t v = do
    e <- mapM lookupSplitEnv v
    case msum e of
        Just b -> return $ pack b
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
