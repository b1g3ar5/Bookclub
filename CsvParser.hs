
module CsvParser
    (
		Value (N, S), csvFile, line, p_value, p_string, p_number, p_empty, parse
    ) where

import Control.Applicative
import Data.Monoid
-- Hide a few names that are provided by Applicative.
import Text.ParserCombinators.Parsec hiding ( many, optional, (<|>))

import Control.Arrow ((>>>))
import Prelude hiding (id)
import Control.Category (id)
import Control.Monad (forM_, MonadPlus(..), ap)
import Control.Arrow (arr, (>>^), (&&&), (>>>), (***), second)
import Data.Monoid (mempty, mconcat)
import qualified Data.Map as M
import Numeric (readSigned, readFloat, readHex)
import System.FilePath (dropExtension, takeFileName) 

-- We have 2 types, string or numbers = Maybe Double
-- So, strings are NOT NULL, NULL = Nothing in the Maybe Double
data Value = S String | N (Maybe Double) deriving (Eq, Ord)

-- Shows just the value or an empty string
instance Show Value where
	show (S s) = s
	show (N (Just v)) = show v
	show (N Nothing) = ""


-- CSV parser definition
csvFile = endBy line eol
line = sepBy p_value (oneOf ",")
--p_value = many (noneOf ",\n\r")
eol = 	try (string "\r\n")
	<|> try (string "\n")
	<|> try (string "\r")
	<?> "end of line"

p_value = value
	where value =   N <$> p_na
				<|>	N <$> p_number
				<|> S <$> p_string
				<?> "csv value"
	
p_na :: CharParser () (Maybe Double)
p_na = try (string "n/a") >>  return Nothing
		<|> empty
		
p_number :: CharParser () (Maybe Double)
p_number = do s <- getInput
              case readSigned readFloat s of
                [(n, s')] -> Just n <$ setInput s'
                _ -> empty
				
p_empty :: CharParser () (Maybe a)
p_empty = do s <- getInput
             case s of
               "" -> Nothing <$ setInput s
               _ -> empty

p_string :: CharParser () String
p_string = many (noneOf ",\n\r")

