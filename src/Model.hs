{-# LANGUAGE NoImplicitPrelude #-}

-- | Database models
--
-- @since 0.1.0
module Model ( UserId, migrateAll ) where

import           ClassyPrelude.Yesod

import           Database.Persist.Quasi

share [ mkPersist sqlSettings, mkMigrate "migrateAll" ]
      $(persistFileWith lowerCaseSettings "config/models.persistentmodels")
