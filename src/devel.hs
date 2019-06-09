-- | The main for development
--
-- @since 0.1.0
module Devel ( main ) where

import           Server ( develMain )

main :: IO ()
main = develMain
