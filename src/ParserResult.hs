-- | Result and state handling
--
-- @since 0.1.0
module ParserResult
    ( ParserResult
    , amount
    , initParserStep
    , parseStepIO
    , removeFromDisk
    , toDisk
    ) where

import           Control.Lens     ( (.~), (?~), (^.), makeLenses )

import           Data.Aeson       ( encodeFile )
import           Data.Aeson.TH
                 ( defaultOptions, deriveJSON, fieldLabelModifier )

import           GoParser         ( parse )

import           Log              ( debug, info, noticeR )

import           Model            ( Benchmark )

import           Parser           ( State(Failure, Init, Ok) )

import           System.Directory ( removePathForcibly )
import           System.IO.Temp   ( emptySystemTempFile )

import           Text.Printf      ( printf )

-- | The result of the complete run
data ParserResult =
    ParserResult { _path       :: Maybe String -- Path on disk
                 , _benchmarks :: [Benchmark]  -- All Benchmark results
                 }

-- | Lens creation
makeLenses ''ParserResult

-- | Drop the underscore from the Result
deriveJSON defaultOptions { fieldLabelModifier = drop 1 } ''ParserResult

-- | A single parser step consists of an intermediate state and result
type ParserStep = (State, ParserResult)

-- | Initial parser step for convenience
initParserStep :: ParserStep
initParserStep = (Init, ParserResult Nothing [])

-- | Go one step forward and log output
parseStepIO :: ParserStep -> String -> IO ParserStep
parseStepIO s line = do
    noticeR line
    let r = parseStep s line
    debugStep r
    return r

-- | Go one step forward by parsing the input String
parseStep :: ParserStep -> String -> ParserStep
parseStep (s, r) i = let ns = parse s i in (ns, appendBenchmark ns r)

-- | Append the succeeding result if possible
appendBenchmark :: State -> ParserResult -> ParserResult
appendBenchmark (Ok b) r = benchmarks .~ (r ^. benchmarks ++ pure b) $ r
appendBenchmark _ r = r

-- | Retrieve the amount of benchmark results for the provided ParserStep
amount :: ParserStep -> Int
amount (_, r) = length $ r ^. benchmarks

-- | Print a debug message for the current step
debugStep :: ParserStep -> IO ()
debugStep (Failure f, r) = do
    info $ printf "Parse error: %s" f
    debugResult r
debugStep (_, r) = debugResult r

-- | Print a debug message for the current result
debugResult :: ParserResult -> IO ()
debugResult r = debug . printf "Result: %s" . show $ r ^. benchmarks

-- | Store the current result on disk
toDisk :: ParserStep -> IO ParserStep
toDisk (s, r) = do
    f <- emptySystemTempFile "result-.json"
    debug $ printf "Writing to temp file: %s" f
    let n = path ?~ f $ r
    encodeFile f n
    return (s, n)

-- | Remove the result from disk
removeFromDisk :: ParserStep -> IO ()
removeFromDisk (_, r) = rm $ r ^. path
  where
    rm (Just x) = removePathForcibly x
    rm _ = return ()
