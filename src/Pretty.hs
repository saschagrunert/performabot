-- | The result pretty printer
--
-- @since 0.1.0
module Pretty ( header, prettyPrint ) where

import           Control.Lens ( (^.) )

import           Data.List    ( find )
import           Data.Text    as T
                 ( Text, append, length, replicate, take, unpack )

import           Model        ( Benchmark, Benchmarks, Test, benchmarkAverage
                              , benchmarkDerivation, benchmarkName
                              , benchmarkSamples, benchmarkUnit, testCommit )

import           Text.Printf  ( printf )

-- | Pretty print the Benchmarks
prettyPrint :: Benchmarks -> Maybe (Test, Benchmarks) -> String
prettyPrint x Nothing = header ++ "Nothing to compare against." ++ nl ++ nl
    ++ tableHeader ++ nl ++ sep ++ unwords (lineR <$> x) ++ nl ++ tableFooter
prettyPrint x (Just (t, b)) = header
    ++ printf "Comparing to commit %s" (T.take 7 $ t ^. testCommit) ++ nl ++ nl
    ++ tableHeader ++ unwords (lineDiff b <$> x) ++ nl ++ tableFooter

-- | Print a line diff between two benchmarks
lineDiff :: Benchmarks -> Benchmark -> String
lineDiff c b = case findName (b ^. benchmarkName) c of
    Nothing -> nl ++ sep ++ lineR b
    Just x -> nl ++ sep ++ line "-" x ++ line "+" b ++ dline x b

-- | Find a matching benchmark by name
findName :: T.Text -> Benchmarks -> Maybe Benchmark
findName n = find (\x -> n == x ^. benchmarkName)

-- | A line for a benchmark without any comparison value
lineR :: Benchmark -> String
lineR = line " "

-- | Create a single result line
line :: String -> Benchmark -> String
line x b = nl ++ printf "%s %s% .3f%s % .3f%s %*d"
                        x
                        (T.unpack . fillOrTrim $ b ^. benchmarkName)
                        (b ^. benchmarkAverage)
                        (b ^. benchmarkUnit)
                        (b ^. benchmarkDerivation)
                        (b ^. benchmarkUnit)
                        (3 :: Int)
                        (b ^. benchmarkSamples)

-- | Print a diff line
dline :: Benchmark -> Benchmark -> String
dline a b = nl ++ printf "= %s% .3f%s % .3f%s %*d"
                         (T.unpack $ fillOrTrim "")
                         (b ^. benchmarkAverage - a ^. benchmarkAverage)
                         (b ^. benchmarkUnit)
                         (b ^. benchmarkDerivation - a ^. benchmarkDerivation)
                         (b ^. benchmarkUnit)
                         (3 :: Int)
                         (b ^. benchmarkSamples - a ^. benchmarkSamples)

-- | Fillup with space or trim string
fillOrTrim :: T.Text -> T.Text
fillOrTrim x
    | T.length x <= maxLength = T.append x $
        T.replicate (maxLength - T.length x) " "
    | otherwise = fillOrTrim $ T.append (T.take (maxLength - 3) x) "…"
  where
    maxLength = 34

-- | The comment header
header :: String
header = "### Performabot Result \129302" ++ nl ++ nl

-- | The table header
tableHeader :: String
tableHeader = "```diff" ++ nl
    ++ "@@                  Performance Diff                    @@" ++ nl
    ++ "##                                        Ø       ±   × ##"

-- | The table footer
tableFooter :: String
tableFooter = sep ++ nl ++ "```"

-- | The separator
sep :: String
sep = "=========================================================="

-- | A newline
nl :: String
nl = "\n"
