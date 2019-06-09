-- | Common handler functions
--
-- @since 0.1.0
module Handler.Common ( getFaviconR ) where

import           Data.FileEmbed ( embedFile )

import           Import

-- | Retrieve the favicon
getFaviconR :: Handler TypedContent
getFaviconR = do
    cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
    return $ TypedContent "image/x-icon" $
        toContent $(embedFile "config/favicon.ico")
