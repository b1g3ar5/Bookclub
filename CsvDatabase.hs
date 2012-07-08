{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances, OverloadedStrings, Arrows #-}

module CsvDatabase
    (
		Record,
		Db,
		LValue (LN, LS),
		exec,
		Col,
		isdouble,
		mfoldl,
		fromDb,
		fromTdb,
		fromCsv,
		Tdb
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

import CsvParser

-- We have 2 types, string or numbers = Maybe Double
-- So, strings are NOT NULL, NULL = Nothing in the Maybe Double
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
		
-------------------------------------------------------------------------
-- Writes a header from the strings of each field
-------------------------------------------------------------------------
write_header::Record->String
write_header r = "<tr>" ++ concatMap (\f ->"<th>" ++ fst f ++ "</th>") (r) ++ "</tr>"
	
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

-- folds a function over a LValue only if it is a Maybe Double list
mfoldl::(Double->Double->Double)->LValue->Double
mfoldl f xs = 	foldl (\a x -> case x of
						Just n -> f a n
						Nothing -> a
				    ) 0.0 ld
				where ld = case xs of
					LN ln -> ln
					LS ls -> [] 

					
{-			
csv :: Csv
csv =
  [ ["Name" , "City"      ]
     -------  ------------                                                      
  , ["Will" , "London"    ]
  , ["John" , "London"    ]
  , ["Chris", "Manchester"]
  , ["Colin", "Liverpool" ]
  , ["Nick" , "London"    ]
  ]
Then, we construct a simple query:

-- "Name"="*i*" @2="London"                                                     
query :: Query
query =
  [ (FieldName "Name", [Wildcard, Char 'i', Wildcard])
  , (FieldIndex 2,
      [Char 'L', Char 'o', Char 'n', Char 'd', Char 'o', Char 'n'])
  ]
And, indeed, running our query against the database yields:

> exec query (fromCsv csv)
[[("Name","Will"),("City","London")],[("Name","Nick"),("City","London")]]					
					
-}