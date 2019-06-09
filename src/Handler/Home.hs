-- | Home handler functions
--
-- @since 0.1.0
module Handler.Home ( getHomeR, postHomeR ) where

import           Import

-- | The Home GET request handler
getHomeR :: Handler Html
getHomeR = do
    (widget, enctype) <- generateFormPost $ renderDivs $ pure ()
    defaultLayout $ do
        setTitle "Home"
        $(widgetFile "home")

-- | The Home POST request handler
postHomeR :: Handler Html
postHomeR = getHomeR
