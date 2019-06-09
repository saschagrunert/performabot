-- | The main server
--
-- @since 0.1.0
module Main ( main ) where

import           Server ( prodMain )

-- | The main function
main :: IO ()
main = prodMain
