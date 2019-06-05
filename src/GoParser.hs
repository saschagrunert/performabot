-- | The golang ginkgo benchmark parser
--
-- @since 0.1.0
module GoParser ( parse ) where

import           Benchmark
                 ( Benchmark(Benchmark), emptyBenchmark, samples )

import           Control.Lens         ( (.~), (^.) )

import           Parser               ( Parser, State(Ok, Failure, NeedMore)
                                      , StringParser, double, integer )

import           Text.Megaparsec
                 ( Token, eof, errorBundlePretty, many, runParser )
import           Text.Megaparsec.Char
                 ( alphaNumChar, char, space1, spaceChar, string )

-- | Parses golang (`ginkgo --succinct`) benchmarks based on the given input
-- state, for example:
--   123 samples:
--     pullTime - Fastest Time: 0.944s, Average Time: 0.953s ± 0.008s, Slowest Time: 0.971s
parse :: State -> String -> State
parse (NeedMore b) i = runStep (step1 b) i
parse _ i = runStep (step0 emptyBenchmark) i

-- | Run a single step abstraction
runStep :: Parser -> String -> State
runStep a i = case runParser a "" i of
    Left e -> Failure $ errorBundlePretty e
    Right r -> r

-- | The initial parse step
step0 :: Benchmark -> Parser
step0 b = do
    space1
    s <- integer
    _ <- string "samples" <* colon <* eof
    return . NeedMore $ samples .~ s $ b

-- | The last parse step
step1 :: Benchmark -> Parser
step1 b = do
    space1
    n <- many alphaNumChar
    _ <- spaceChar <* char '-' <* spaceChar
    _ <- string "Fastest" <* spaceChar <* string "Time" <* colon
    _ <- spaceChar <* double <* s <* char ',' <* spaceChar
    _ <- string "Average" <* spaceChar <* string "Time" <* colon <* spaceChar
    a <- double
    _ <- s <* spaceChar <* char '±' <* spaceChar
    d <- double
    _ <- s <* char ',' <* spaceChar <* string "Slowest" <* spaceChar
    _ <- string "Time" <* colon <* spaceChar <* double <* s <* eof
    return . Ok $ Benchmark a d n (b ^. samples) "s"
  where
    s = char 's'

-- | Parses a single colon
colon :: StringParser (Token String)
colon = char ':'
