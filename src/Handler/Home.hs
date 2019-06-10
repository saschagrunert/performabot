-- | Home handler functions
--
-- @since 0.1.0
module Handler.Home ( getHomeR ) where

import           Import

-- | The Home GET request handler
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Home"
    $(widgetFile "home")
