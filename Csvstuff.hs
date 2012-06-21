{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances, OverloadedStrings, Arrows #-}

module Csvstuff
    (
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

import CsvParser
import CsvDatabase

			  
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
		
					
-- Some functions to play with columns
	
-- Reads a strings to Maybe Double
dread::String->Maybe Double
dread s = case readFloat s of
			[(n,s')]-> Just n
			_ -> Nothing
	

msum = mapply (+)					
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
		

