-- | System Environment handling
--
-- @since 0.1.0
module Environment ( fillEnvironment ) where

import           Control.Lens       ( (.~), (^.) )
import           Control.Monad      ( mapM, msum )

import           Data.List          ( intercalate )
import           Data.Text          ( Text, pack )
import qualified Data.Text          as T ( null )

import           Log                ( err )

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
    t <- getToken (e ^. environmentToken)
    if not d && any T.null [ b, c, p, t ]
        then exitFailure
        else return $ environmentToken .~ t $ environmentPullRequest .~ p $
            environmentCommit .~ c $ environmentBranch .~ b $ e

-- | The prefix for local env vars
prefix :: String -> String
prefix = (++) "PB_"

-- | Possible token environment variables sorted by priority
tokenEnvVar :: String
tokenEnvVar = prefix "TOKEN"

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

-- | Retrieve the token environment value
getToken :: Text -> IO Text
getToken "" = do
    e <- lookupEnv tokenEnvVar
    case e of
        Just t -> return $ pack t
        _ -> do
            err $ printf "No token found via $%s environment variable"
                         tokenEnvVar
            return ""

getToken x = return x

-- | Generic environment variable retrieval
getEnv :: Text -> String -> [String] -> IO Text
getEnv "" t v = do
    e <- mapM lookupEnv v
    case msum e of
        Just b -> return $ pack b
        _ -> do
            err . printf ("No %s found via the $%s environment "
                          ++ "variables or the command line")
                         t $ intercalate "/$" v
            return ""

getEnv x _ _ = return x
