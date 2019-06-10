{-# LANGUAGE NoImplicitPrelude #-}

-- | Main API handler
--
-- @since 0.1.0
module Handler.Api ( getApiR, postApiR ) where

import           Data.Aeson.Types as T ( Result(Success) )

import           Import

getApiR :: Handler Value
getApiR = return $ String "Hello world"

postApiR :: Handler ()
postApiR = do
    body <- parseCheckJsonBody :: Handler (T.Result Value)
    case body of
        Success _ -> sendResponse . toTypedContent . toJSON $ emptyBenchmark
        _ -> sendResponseStatus status400 ()
