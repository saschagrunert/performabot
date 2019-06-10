{-# LANGUAGE NoImplicitPrelude #-}

-- | Database models
--
-- @since 0.1.0
module Model ( User, UserId, migrateAll ) where

import           ClassyPrelude.Yesod    ( Text, mkMigrate, mkPersist
                                        , persistFileWith, share, sqlSettings )

import           Database.Persist.Quasi ( lowerCaseSettings )

share [ mkPersist sqlSettings, mkMigrate "migrateAll" ]
      $(persistFileWith lowerCaseSettings "config/models.persistentmodels")
