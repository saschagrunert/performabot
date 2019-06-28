-- | Result and state handling
--
-- @since 0.1.0
module Result ( amount, initParserStep, parseStepIO, save ) where

import           Control.Lens            ( (^.) )
import           Control.Monad.IO.Class  ( liftIO )

import           Data.Text               ( Text, pack )
import           Data.Time.Clock         ( getCurrentTime )

import           Database.Persist        ( (<-.), (==.), SelectOpt(Asc, LimitTo)
                                         , entityVal, insert, selectList )
import           Database.Persist.Sqlite ( runMigration, runSqlite )

import           Environment
                 ( Environment, commit, owner, pullRequest, repository )

import           Github                  ( baseCommit, comment )

import           Log                     ( debug, info, notice, noticeR )

import           Model
                 ( Benchmarks, EntityField(TestCommit, BenchmarkId, TestTime)
                 , Test(Test), migrateAll, testBenchmarks )

import           Parser                  ( State(Failure, Init, Ok) )

import qualified ParserGo                as Go ( parse )

import           Pretty                  ( prettyPrint )

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
    debug $ printf "Parse error:\n%s" f
    debugResult r
debugStep (_, r) = debugResult r

-- | Print a debug message for the current result
debugResult :: Benchmarks -> IO ()
debugResult r = debug . printf "Current result: %s" $ show r

-- | Sen the provided data to the given url including the environment
save :: Step -> Environment -> IO ()
save (_, b) e = do
    c <- baseCommit e
    info "Base commit retrieval successful"
    insertInDB e b
    info "Database insertion successful"
    pb <- entryForCommit c
    let r = prettyPrint b pb
    notice $ printf "The report: %s" r
    comment e r

-- | The database name
db :: Text
db = "performabot.sqlite"

-- | Insert the test into the database
insertInDB :: Environment -> Benchmarks -> IO ()
insertInDB e b = runSqlite db $ do
    runMigration migrateAll
    t <- liftIO getCurrentTime
    bids <- mapM insert b
    _ <- insert $ Test t
                       (pack $ e ^. commit)
                       (pack $ e ^. repository)
                       (pack $ e ^. owner)
                       (pack $ e ^. pullRequest)
                       bids
    return ()

-- | Try to retrieve a test for a given commit
entryForCommit :: Text -> IO (Maybe (Test, Benchmarks))
entryForCommit c = runSqlite db $ do
    runMigration migrateAll
    t <- selectList [ TestCommit ==. c ] [ LimitTo 1, Asc TestTime ]
    case t of
        [ i ] -> do
            liftIO . debug . printf "Base test entry: %s" $ show i
            let v = entityVal i
            bs <- selectList [ BenchmarkId <-. v ^. testBenchmarks ] []
            let b = entityVal <$> bs
            liftIO . debug . printf "Base test benchmarks: %s" $ show b
            return $ Just (v, b)
        _ -> do
            liftIO $ debug "No last test entry found"
            return Nothing
