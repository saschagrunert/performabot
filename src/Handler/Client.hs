{-# LANGUAGE NoImplicitPrelude #-}

-- | Client API handler
--
-- @since 0.1.0
module Handler.Client ( postClientR ) where

import           Data.Aeson.Types as T ( Result(Success) )
import           Data.Time.Clock  ( getCurrentTime )

import           Import

postClientR :: Handler ()
postClientR = do
    body <- parseCheckJsonBody :: Handler (T.Result ReqBody)
    case body of
        Success (e, b) -> do
            eId <- runDB $ insert e
            bIds <- runDB $ mapM insert b
            time <- liftIO getCurrentTime
            _ <- runDB . insertEntity $ Test bIds eId time
            sendResponseStatus status200 ()
        _ -> sendResponseStatus status400 ()
