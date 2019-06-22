{-# LANGUAGE NoImplicitPrelude #-}

-- | GitHub API handler
--
-- @since 0.1.0
module Handler.GitHub ( postGitHubR ) where

import           Import

postGitHubR :: Handler ()
postGitHubR = sendResponseStatus status200 ()
