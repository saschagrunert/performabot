-- | System environment handling
--
-- @since 0.1.0
module Env
    ( branchEnvVars
    , commitEnvVars
    , fillEnvironment
    , pullRequestEnvVars
    , tokenEnvVars
    ) where

import           Control.Lens       ( (.~), (^.) )
import           Control.Monad      ( mapM, msum, unless )

import           Data.List          ( intercalate )
import           Data.Maybe         ( isNothing )
import           Data.Text          ( Text, pack )
import qualified Data.Text          as T ( null )
import           Data.UUID          ( fromText )

import           Log                ( debug, err )

import           Model
                 ( Environment, environmentBranch, environmentCommit
                 , environmentPullRequest, environmentToken )

import           System.Environment ( lookupEnv )
import           System.Exit        ( exitFailure )

import           Text.Printf        ( printf )

fillEnvironment :: Environment -> Bool -> IO Environment
fillEnvironment e d = do
    b <- getEnv (e ^. environmentBranch) "branch" branchEnvVars
    c <- getEnv (e ^. environmentCommit) "commit" commitEnvVars
    p <- getEnv (e ^. environmentPullRequest) "pull request" pullRequestEnvVars
    t <- getEnv (e ^. environmentToken) "token" tokenEnvVars

    -- Validate the token
    unless (T.null t) $ if isNothing (fromText t)
                        then do
                            err $ printf "Invalid token '%s' provided" t
                            exitFailure
                        else debug $ printf "Token '%s' seems to be valid" t

    -- Validate the other environment variables
    if not d && any T.null [ b, c, p, t ]
        then exitFailure
        else return $ environmentToken .~ t $ environmentPullRequest .~ p $
            environmentCommit .~ c $ environmentBranch .~ b $ e

-- | The prefix for local env vars
prefix :: String -> String
prefix = (++) "PB_"

-- | Possible token environment variables sorted by priority
tokenEnvVars :: [String]
tokenEnvVars = [ prefix "TOKEN" ]

-- | Possible branch environment variables sorted by priority
branchEnvVars :: [String]
branchEnvVars = [ prefix "BRANCH", "CIRCLE_BRANCH", "TRAVIS_BRANCH" ]

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
