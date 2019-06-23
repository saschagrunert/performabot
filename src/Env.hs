-- | System environment handling
--
-- @since 0.1.0
module Env
    ( commitEnvVars
    , fillEnvironment
    , pullRequestEnvVars
    , repositoryEnvVars
    , tokenEnvVars
    ) where

import           Control.Lens       ( (.~), (^.) )
import           Control.Monad      ( mapM, msum )

import           Data.List          ( intercalate )
import           Data.Text          ( Text, pack )
import qualified Data.Text          as T ( null )

import           Log                ( err )

import           Model
                 ( Environment, environmentCommit, environmentPullRequest
                 , environmentRepository, environmentToken )

import           System.Environment ( lookupEnv )
import           System.Exit        ( exitFailure )

import           Text.Printf        ( printf )

fillEnvironment :: Environment -> Bool -> IO Environment
fillEnvironment e d = do
    c <- getEnv (e ^. environmentCommit) "commit" commitEnvVars
    p <- getEnv (e ^. environmentPullRequest) "pull request" pullRequestEnvVars
    r <- getEnv (e ^. environmentRepository) "repository" repositoryEnvVars
    t <- getEnv (e ^. environmentToken) "token" tokenEnvVars

    -- Validate the other environment variables
    if not d && any T.null [ r, c, p, t ]
        then exitFailure
        else return $ environmentToken .~ t $ environmentPullRequest .~ p $
            environmentCommit .~ c $ environmentRepository .~ r $ e

-- | The prefix for local env vars
prefix :: String -> String
prefix = (++) "PB_"

-- | Possible token environment variables sorted by priority
tokenEnvVars :: [String]
tokenEnvVars = [ prefix "TOKEN" ]

-- | Possible repository environment variables sorted by priority
repositoryEnvVars :: [String]
repositoryEnvVars =
    [ prefix "REPOSITORY", "CIRCLE_PROJECT_REPONAME", "TRAVIS_REPO_SLUG" ]

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
    e <- mapM lookupEnv v
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
