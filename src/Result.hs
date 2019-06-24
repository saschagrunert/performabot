-- | Result and state handling
--
-- @since 0.1.0
module Result ( Result, amount, initParserStep, parseStepIO, save ) where

import           Control.Lens    ( (^.) )

import           Data.Aeson      ( encodeFile )
import           Data.Text       ( pack )
import           Data.Time.Clock ( getCurrentTime )

import           Environment     ( Environment, commit, pullRequest, repoSlug )

import           Log             ( debug, notice, noticeR )

import           Model           ( Benchmark, Test(Test) )

import           Parser          ( State(Failure, Init, Ok) )

import qualified ParserGo        as Go ( parse )

import           System.IO.Temp  ( emptySystemTempFile )

import           Text.Printf     ( printf )

-- | The result of the complete run
type Result = [Benchmark]

-- | A single parser step consists of an intermediate state and result
type Step = (State, Result)

-- | Initial parser step for convenience
initParserStep :: Step
initParserStep = (Init, [])

-- | Go one step forward and log output
parseStepIO :: Step -> String -> IO Step
parseStepIO s line = do
    noticeR line
    let r = parseStep s line
    debugStep r
    return r

-- | Go one step forward by parsing the input String
parseStep :: Step -> String -> Step
parseStep (s, r) i = let ns = Go.parse s i in (ns, appendBenchmark ns r)

-- | Append the succeeding result if possible
appendBenchmark :: State -> Result -> Result
appendBenchmark (Ok b) r = r ++ pure b
appendBenchmark _ r = r

-- | Retrieve the amount of benchmark results for the provided Step
amount :: Step -> Int
amount (_, r) = length r

-- | Print a debug message for the current step
debugStep :: Step -> IO ()
debugStep (Failure f, r) = do
    debug $ printf "Parse error: %s" f
    debugResult r
debugStep (_, r) = debugResult r

-- | Print a debug message for the current result
debugResult :: Result -> IO ()
debugResult r = debug . printf "Current result: %s" $ show r

-- | Store the current result on disk
toDisk :: (Test, Result) -> IO FilePath
toDisk b = do
    f <- emptySystemTempFile "result-.json"
    debug $ printf "Writing to temp file: %s" f
    encodeFile f b
    return f

-- | Sen the provided data to the given url including the environment
save :: Step -> Environment -> IO ()
save (_, r) e = do
    t <- getCurrentTime
    let d = ( Test t
                   (pack $ e ^. commit)
                   (pack $ e ^. repoSlug)
                   (pack $ e ^. pullRequest)
                   []
            , r
            )
    p <- toDisk d
    logFilePath p
    return ()

-- | Log the file path convenience function
logFilePath :: FilePath -> IO ()
logFilePath p = notice $ printf "You can retry by using the file %s" p
