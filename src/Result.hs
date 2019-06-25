-- | Result and state handling
--
-- @since 0.1.0
module Result ( amount, initParserStep, parseStepIO, save ) where

import           Control.Lens            ( (^.) )

import           Data.Text               ( Text, pack )
import           Data.Time.Clock         ( getCurrentTime )

import           Database.Persist        ( insert )
import           Database.Persist.Sqlite ( runMigration, runSqlite )

import           Environment
                 ( Environment, commit, pullRequest, repoSlug )

import           Log                     ( debug, noticeR )

import           Model                   ( Benchmarks, Test(Test), migrateAll )

import           Parser                  ( State(Failure, Init, Ok) )

import qualified ParserGo                as Go ( parse )

import           Text.Printf             ( printf )

-- | A single parser step consists of an intermediate state and result
type Step = (State, Benchmarks)

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
appendBenchmark :: State -> Benchmarks -> Benchmarks
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
debugResult :: Benchmarks -> IO ()
debugResult r = debug . printf "Current result: %s" $ show r

-- | Sen the provided data to the given url including the environment
save :: Step -> Environment -> IO ()
save (_, b) e = do
    t <- getCurrentTime
    _ <- runSqlite db $ do
        runMigration migrateAll
        bids <- mapM insert b
        insert $ Test t
                      (pack $ e ^. commit)
                      (pack $ e ^. repoSlug)
                      (pack $ e ^. pullRequest)
                      bids
    return ()

-- | The database name
db :: Text
db = "performabot.sqlite"
