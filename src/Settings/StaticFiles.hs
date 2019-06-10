-- | Static files manipulation
--
-- @since 0.1.0
module Settings.StaticFiles ( css_bulma_min_css ) where

import           Control.Lens ( (^.) )

import           Settings     ( appStaticDir, compileTimeAppSettings )

import           Yesod.Static ( staticFiles )

-- | This generates easy references to files in the static directory at compile
-- time, giving you compile-time verification that referenced files exist.
--
-- Warning: any files added to your static directory during run-time can't be
-- accessed this way. You'll have to use their FilePath or URL to access them.
--
-- For example, to refer to @static/js/script.js@ via an identifier, you'd use:
--
--     js_script_js
--
-- If the identifier is not available, you may use:
--
--     StaticFile ["js", "script.js"] []
staticFiles $ compileTimeAppSettings ^. appStaticDir
