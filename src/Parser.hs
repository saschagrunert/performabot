-- | All parser relevant implementations which are shared by multiple dedicated
-- implementations
--
-- @since 0.1.0
module Parser
    ( Parser
    , State(Init, NeedMore, Ok, Failure)
    , StringParser
    , double
    , integer
    ) where

import           Benchmark                  ( Benchmark )

import           Data.Void                  ( Void )

import           Text.Megaparsec            ( Parsec, empty )
import           Text.Megaparsec.Char       ( space1 )
import qualified Text.Megaparsec.Char.Lexer as L
                 ( decimal, float, lexeme, space )

-- | All possible parser states
data State = Init               -- The start phase of the phase
           | NeedMore Benchmark -- Indicates that more input is needed
           | Ok Benchmark       -- The parser is done
           | Failure String     -- The parser failed
    deriving ( Show )

-- | The generic string input parser
type StringParser = Parsec Void String

-- | The benchmark parser
type Parser = StringParser State

-- | Consumes all space
spaceConsumer :: StringParser ()
spaceConsumer = L.space space1 empty empty

-- | The lexer based on the spaceConsumer
lexeme :: StringParser a -> StringParser a
lexeme = L.lexeme spaceConsumer

-- | Parses integer numbers
integer :: StringParser Integer
integer = lexeme L.decimal

-- | Parses double numbers
double :: StringParser Double
double = lexeme L.float
