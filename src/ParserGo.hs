-- | The golang ginkgo benchmark parser
--
-- @since 0.1.0
module ParserGo ( parse ) where

import           Control.Lens              ( (.~), (^.) )
import           Control.Monad.Combinators ( skipMany )

import           Data.Text                 ( pack )

import           Model
                 ( Benchmark(Benchmark), benchmarkSamples, emptyBenchmark )

import           Parser
                 ( Parser, State(Ok, Failure, NeedMore), StringParser, double
                 , integer )

import           Text.Megaparsec
                 ( anySingle, eof, errorBundlePretty, manyTill, runParser )
import           Text.Megaparsec.Char
                 ( char, numberChar, space, space1, spaceChar, string )

-- | Parses golang (`ginkgo --succinct`) benchmarks based on the given input
-- state, for example:
--   123 samples:
--     bench1 - Fastest Time: 0.944s, Average Time: 0.953s ± 0.008s, Slowest Time: 0.971s
--     bench2 - Fastest Time: 0.944s, Average Time: 0.953s ± 0.008s, Slowest Time: 0.971s
parse :: State -> String -> State
parse (NeedMore b) i = runStep (step1 b) i
parse (Ok b) i = runStep (step1 b) i
parse _ i = runStep (step0 emptyBenchmark) i

-- | Run a single step abstraction
runStep :: Parser -> String -> State
runStep a i = case runParser a "" i of
    Left e -> Failure $ errorBundlePretty e
    Right r -> r

-- | The initial parse step
step0 :: Benchmark -> Parser
step0 b = do
    _ <- space1 <* ansi
    s <- integer
    _ <- ansi <* space <* string "samples:" <* eof
    return . NeedMore $ benchmarkSamples .~ s $ b

-- | The last parse step
step1 :: Benchmark -> Parser
step1 b = do
    _ <- space1 <* ansi
    n <- manyTill anySingle $ ansi <* string " - Fastest Time: "
    _ <- ansi <* double <* ansi <* s <* char ',' <* spaceChar
        <* string "Average Time: " <* ansi
    a <- double
    _ <- ansi <* s <* spaceChar <* char '±' <* spaceChar <* ansi
    d <- double
    _ <- ansi <* s <* string ", Slowest Time: " <* ansi <* double <* ansi <* s
        <* eof
    return . Ok $ Benchmark a d (pack n) (b ^. benchmarkSamples) "s"
  where
    s = char 's'

-- | Parses ansi escape sequences
ansi :: StringParser ()
ansi = skipMany $ char '\ESC' <* char '[' <* manyTill numberChar (char 'm')
