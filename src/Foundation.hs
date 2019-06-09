{-# LANGUAGE NoImplicitPrelude #-}

-- | The foundation of the Server
--
-- @since 0.1.0
module Foundation ( App(..), Handler, Route(..), resourcesApp ) where

import           Control.Lens         ( (^.) )
import           Control.Monad.Logger ( LogSource )

import           Database.Persist.Sql ( ConnectionPool, runSqlPool )

import           Import.NoFoundation

import           Text.Hamlet          ( hamletFile )
import           Text.Jasmine         ( minifym )

import           Yesod.Core.Types     ( Logger )
import           Yesod.Default.Util   ( addStaticContentExternal )

-- | The main application
data App = App { appSettings    :: AppSettings
               , appStatic      :: Static
               , appConnPool    :: ConnectionPool
               , appHttpManager :: Manager
               , appLogger      :: Logger
               }

-- | A single navigation item
data NavItem = NavItem { _navItemLabel         :: Text
                       , _navItemRoute         :: Route App
                       , navItemAccessCallback :: Bool
                       }

-- | Navigation types to distinguish left from right
data NavTypes = NavbarLeft NavItem | NavbarRight NavItem

mkYesodData "App" $(parseRoutesFile "config/routes")

-- The main yesod instance typeclass
instance Yesod App where
    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = return Nothing

    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO _ _ _ = return True

    isAuthorized :: Route App -> Bool -> Handler AuthResult
    isAuthorized FaviconR _ = return Authorized
    isAuthorized _ _ = return Authorized

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        mcurrentRoute <- getCurrentRoute
        -- specify the navigation items
        let navItems = [ NavbarLeft $ NavItem "Home" HomeR True ]
        let navbarLeftItems = [ x | NavbarLeft x <- navItems ]
        let navbarRightItems = [ x | NavbarRight x <- navItems ]
        let navbarLeftFilteredItems =
                [ x | x <- navbarLeftItems, navItemAccessCallback x ]
        let navbarRightFilteredItems =
                [ x | x <- navbarRightItems, navItemAccessCallback x ]
        -- set the page content
        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_uikit_min_css
            addScript $ StaticR js_uikit_min_js
            addScript $ StaticR js_uikit_icons_min_js
            $(widgetFile "layout")
        withUrlRenderer $(hamletFile "templates/wrapper.hamlet")

    addStaticContent
        :: Text
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appSettings master ^. appStaticDir
        addStaticContentExternal minifym
                                 genFileName
                                 staticDir
                                 (StaticR . flip StaticRoute [])
                                 ext
                                 mime
                                 content
      where
        genFileName lbs = "gen-" ++ base64md5 lbs

-- | Useful when writing code that is re-usable outside of the Handler context
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

-- | Use the standard English messages. Supply a translating function for i18n
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | How to run database actions
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend

    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool
