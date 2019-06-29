-- | Default data and paths
--
-- @since 0.1.0
module Default ( db, repo ) where

import           Data.Text ( Text )

-- | The database name
db :: Text
db = "performabot.sqlite"

-- | The default repo name used for storage
repo :: Text
repo = "performabot-results"
