-- | System Environment handling
--
-- @since 0.1.0
module Environment ( isCI ) where

import           Control.Monad      ( mapM )

import           Data.Maybe         ( isJust )

import           System.Environment ( lookupEnv )

-- | Returns true if the current run is within a CI environment
isCI :: IO Bool
isCI = or . fmap isJust <$> mapM lookupEnv [ "TRAVIS", "CIRCLE" ]
-- | Environment variables to be parsed
--
-- Travis:
-- TRAVIS_BRANCH
-- TRAVIS_COMMIT
-- TRAVIS_PULL_REQUEST
--
-- CircleCI:
-- CIRCLE_BRANCH
-- CIRCLE_SHA1
-- CIRCLE_PR_NUMBER
--
-- General:
-- TOKEN
