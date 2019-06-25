-- | The golang ginkgo benchmark parser
--
-- @since 0.1.0
module ParserGo ( parse ) where

import           Control.Lens         ( (.~), (^.) )

import           Data.Text            ( pack )

import           Model
                 ( Benchmark(Benchmark), benchmarkSamples, emptyBenchmark )

import           Parser               ( Parser, State(Ok, Failure, NeedMore)
                                      , StringParser, double, integer )

import           Text.Megaparsec      ( Token, anySingle, eof, errorBundlePretty
                                      , manyTill, runParser )
import           Text.Megaparsec.Char ( char, space1, spaceChar, string )
import           Text.Regex           ( mkRegex, subRegex )

-- | Parses golang (`ginkgo --succinct`) benchmarks based on the given input
-- state, for example:
--   123 samples:
--     bench1 - Fastest Time: 0.944s, Average Time: 0.953s ± 0.008s, Slowest Time: 0.971s
--     bench2 - Fastest Time: 0.944s, Average Time: 0.953s ± 0.008s, Slowest Time: 0.971s
parse :: State -> String -> State
parse (NeedMore b) i = runStep1 b i
parse (Ok b) i = runStep1 b i
parse _ i = runStep (step0 emptyBenchmark) i
  where
    runStep1 b = runStep (step1 b)

-- | Run a single step abstraction
runStep :: Parser -> String -> State
runStep a i = case runParser a "" (ansiFilter i) of
    Left e -> Failure $ errorBundlePretty e
    Right r -> r

-- | Strip all colors from the string
ansiFilter :: String -> String
ansiFilter line = subRegex (mkRegex "\\[[0-9]+m") stripped ""
  where
    stripped = filter (/= '\ESC') line

-- | The initial parse step
step0 :: Benchmark -> Parser
step0 b = do
    space1
    s <- integer
    _ <- string "samples" <* colon <* eof
    return . NeedMore $ benchmarkSamples .~ s $ b

-- | The last parse step
step1 :: Benchmark -> Parser
step1 b = do
    space1
    n <- manyTill anySingle $ string " - "
    _ <- string "Fastest" <* spaceChar <* string "Time" <* colon
    _ <- spaceChar <* double <* s <* char ',' <* spaceChar
    _ <- string "Average" <* spaceChar <* string "Time" <* colon <* spaceChar
    a <- double
    _ <- s <* spaceChar <* char '±' <* spaceChar
    d <- double
    _ <- s <* char ',' <* spaceChar <* string "Slowest" <* spaceChar
    _ <- string "Time" <* colon <* spaceChar <* double <* s <* eof
    return . Ok $ Benchmark a d (pack n) (b ^. benchmarkSamples) "s"
  where
    s = char 's'

-- | Parses a single colon
colon :: StringParser (Token String)
colon = char ':'
