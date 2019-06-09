-- | Main API handler
--
-- @since 0.1.0
module Handler.Api ( getApiR, postApiR ) where

import           Data.Aeson.Types ( Result(Success) )

import           Import

getApiR :: Handler Value
getApiR = return $ String "Hello world"

postApiR :: Handler ()
postApiR = do
    body <- parseCheckJsonBody :: Handler (Result Value)
    case body of
        Success v -> sendResponse v
        _ -> sendResponseStatus status400 ()
