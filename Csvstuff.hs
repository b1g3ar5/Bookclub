{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances, OverloadedStrings, Arrows #-}

module Csvstuff
    (
		Csv,
		Db,
		Tdb,
		Record,
		Field,
		fromCsv,
		fromDb,
		fromTdb,
		Query,
		Filter,
		Selector,
		ValueFilter,
		CParser,
		cparse,
		filterValue,
		select,
		apply,
		exec,
		readQuery,
		csvFile,
		dread,
		msum,
		msumsq,
		mcount,
		mmin,
		mmax,
		mmean,
		mstd,
		rownames,
		colnames,
		Col,
		line,
		p_value,
		p_string,
		p_number,
		p_empty,
		isdouble
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
data LValue = LS [String] | LN [Maybe Double] deriving (Eq, Ord)

-- Overide the standard show
instance Show (Maybe Double) where
	show (Just v) = show v
	show Nothing = ""
	
instance Monoid LValue where
	mappend l1 l2 = case (l1, l2) of
					(LN ln1, LN ln2) -> LN (ln1++ln2)
					(LN ln1, LS ls2) -> LS ((map show ln1)++ls2)
					(LS ls1, LN ln2) -> LS (ls1++ (map show ln2))
					(LS ls1, LS ls2) -> LS (ls1++ls2)
	mempty = LN []

-- Shows the LValue in a column - for the xml file for charts package
-- Shows the strings for when it's eg the names of the books
-- Shows the numbers for when it's eg the scores
instance Show LValue where
	show (LN ln) = concatMap (\md-> "<number>" ++ show md ++ "</number>" ) ln
	show (LS ls) = concatMap (\ms-> "<string>" ++ ms ++ "</string>" ) ls	

instance Show Value where
	show (S s) = s
	show (N (Just v)) = show v
	show (N Nothing) = ""

	
type Csv = [[Value]]
type Db = [Record]
type Record = [Field]
type Field = (String, Value)	-- key, value
type Col = (String, LValue)   	-- ie. all the same field - we need to make them all doubles or all strings...
type Tdb = [Col]

-- Writes a Db as a html table
instance Show Db where 
	show db = "<table>" ++ (write_header (head db)) ++ (concatMap show (tail db)) ++ "</table>"

-- Writes a Record as a html table-record
-- where each field is a <td>
instance Show Record where
	show r = "<tr>" ++ concatMap (\f -> "<td>" ++ show (snd f) ++ "</td>") r ++ "</tr>"
		
-- Add a field to a column
-- If one of the fields is a string then we get a sring
addField::Field->Col->Col
addField fld col = case (fst fld == fst col) of
						True -> ( fst fld, lv `mappend` snd col )
								where lv = case (snd fld) of 
												N n -> LN [n]
												S s -> LS [s]
						False -> col -- adds nothing

addcc::Col->Col->Col
addcc c1 c2 = (fst c1, (snd c1) `mappend` (snd c2))
		
-- Add a Record to a Tdb (transposed Db)
addRecord::Record->Tdb->Tdb
addRecord r tdb = zipWith addcc (rec2tdb r) tdb
						
-- Turn a field into a column
field2col::Field->Col
field2col f = (fst f, lv)
				where lv = case snd f of
						N n -> LN [n]
						S s -> LS [s]
					

-- Turn a record into a Tdb						
rec2tdb::Record->Tdb
rec2tdb = map field2col						

-- Turn a Db into a Tdb
fromDb::Db->Tdb
fromDb db = foldl (flip addRecord) (rec2tdb $ head db) (tail db)
		
--Turn a Tdb into a Db
fromTdb::Tdb->Db
fromTdb [] = []
fromTdb tdb = case snd $ head tdb of
				LN (h:ts)->[(map (\c->(fst c, N h)) tdb)] ++ (fromTdb (map (\c->(fst c, LN ts)) tdb))
				LN []   ->[]
				LS (h:ts)->[(map (\c->(fst c, S h)) tdb)] ++ (fromTdb (map (\c->(fst c, LS ts)) tdb))
				LS []   ->[]

isdouble::Col->Bool
isdouble c = case (snd c) of
				LN n -> True
				LS s -> False
				

-- The headers are the first row, turned into strings, if not already
fromCsv::Csv->Db
fromCsv [] =  []
fromCsv (h:rs) =  map (\r-> zipWith (\hf rf ->
											case hf of
												S shf->(shf, rf)
												N dhf->(show dhf, rf)
									) h r) rs

type Query = [Filter]
type Filter = (Selector, ValueFilter)
data Selector = FieldName String | FieldIndex Int

type ValueFilter = [CParser]
data CParser = Char Char | Wildcard

cparse :: CParser -> String -> [String]
cparse (Char c) (c' : cs') | c == c' = [cs']
cparse Wildcard []                   = [[]]
cparse Wildcard cs@(_ : cs')         = cs : cparse Wildcard cs'
cparse _ _                           = []
	
-- Only implemented for S = string Values
filterValue :: ValueFilter -> Value -> Bool
filterValue ps (S cs) = any null (go ps cs)
  where
    go [] cs       = [cs]
    go (p : ps) cs = concatMap (go ps) (cparse p cs)

select :: Selector -> Record -> Maybe Value
select (FieldName s) r                           = lookup s r
select (FieldIndex n) r | n > 0 && n <= length r = Just (snd (r !! (n - 1)))
                        | otherwise              = Nothing


apply :: Filter -> Record -> Bool
apply (s, vf) r = case select s r of
  Nothing -> False
  Just v  -> filterValue vf v
  
exec :: Query -> Db -> [Record]
exec = (flip . foldl . flip) (filter . apply)

readQuery :: String -> Maybe Query
readQuery = undefined  



						
------------------------------------------------
-- This code parses the CSV file to a [[String]]		
------------------------------------------------
		
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


			  
-------------------------------------------------------------------------
-- Writes a header from the strings of each field
-------------------------------------------------------------------------
write_header::Record->String
write_header r = "<tr>" ++ concatMap (\f ->"<th>" ++ fst f ++ "</th>") (r) ++ "</tr>"
	
{-------------------------------------------
Operations on a Db
--------------------------------------------}
	
-- Takes the keys as the column names
colnames::Db->[String]
colnames = map fst . head

-- Takes the first column as the row names
rownames::Db->[String]
rownames = map (show . snd . head) . tail

-- The data rows - ie tail!
rows::Db->Db
rows = tail

-- The data columns
columns::Db->Db
columns db = tail $ foldl add_on (map (\x->[x]) (head $ tail db)) (tail $ tail db)
				where
					add_on xss ys = zipWith (\xs y -> xs++[y]) xss ys
		
				
-- Convert a column to numbers	
dlread::[String]->[Maybe Double]
dlread = map dread
	
-- Some functions to play with columns
	
-- Reads a strings to Maybe Double
dread::String->Maybe Double
dread s = case readFloat s of
			[(n,s')]-> Just n
			_ -> Nothing
	
-- Sums a list of Maybe Doubles
msum::LValue->Double	
msum xs = foldl (\a x -> case x of
						Just n -> a+n
						Nothing -> a
				) (0.0) ld
			where ld = case xs of
						LN ln -> ln
						LS ls -> []
	
-- folds a function over a LValue only if it is a Maybe Double list
mapply::(Double->Double->Double)->LValue->Double
mapply f xs = 	foldl (\a x -> case x of
						Just n -> f a n
						Nothing -> a
				    ) 0.0 ld
				where ld = case xs of
					LN ln -> ln
					LS ls -> [] 
mmax = mapply max
mmin = mapply min
msumsq = mapply (\a n -> a+n*n)
mcount = mapply (\a n -> a+1.0)
	
mmean::LValue->Double	
mmean xs = (msum xs)/(mcount xs)
	
mvar::LValue->Double	
mvar xs = (msumsq xs)/(mcount xs) - (mmean xs)**2.0
		
mstd::LValue->Double	
mstd xs = (mvar xs)**0.5
		

{-
mmax::LValue->Double	
mmax xs = foldl (\a x -> case x of
						Just n -> max a n
						Nothing -> a
				) (0.0) ld
			where ld = case xs of
						LN ln -> ln
						LS ls -> [] 
	
mmin::LValue->Double	
mmin xs = foldl (\a x -> case x of
						Just n -> min a n
						Nothing -> a
				) (10.0) ld
			where ld = case xs of
						LN ln -> ln
						LS ls -> [] 
	
msumsq::LValue->Double	
msumsq xs = foldl (\a x -> case x of
						Just n -> a+n*n
						Nothing -> a
				) (0.0) ld
			where ld = case xs of
						LN ln -> ln
						LS ls -> [] 
	
mcount::LValue->Double	
mcount xs = foldl (\a x -> case x of
						Just n -> a+1.0
						Nothing -> a
				) (0.0) ld
			where ld = case xs of
						LN ln -> ln
						LS ls -> [] 
-}
	
