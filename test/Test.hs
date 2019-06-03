module Main ( main ) where

import           Lib         ( parse )

import           System.Exit ( exitFailure, exitSuccess )

main :: IO ()
main = do
    let success = parse "test"
    if success then exitSuccess else exitFailure
