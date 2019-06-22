{-# LANGUAGE NoImplicitPrelude #-}

-- | GitHub API handler
--
-- @since 0.1.0
module Handler.GitHub ( postGitHubR ) where

import           Data.Aeson.Types            as T ( Result(Success) )

import           GitHub.Data.Webhooks.Events ( PullRequestEvent )

import           Import

postGitHubR :: Handler ()
postGitHubR = do
    body <- parseCheckJsonBody :: Handler (T.Result PullRequestEvent)
    case body of
        Success _ -> sendResponseStatus status200 ()
        _ -> sendResponseStatus status400 ()
