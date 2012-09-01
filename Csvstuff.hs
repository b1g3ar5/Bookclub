{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances, OverloadedStrings, Arrows #-}

module Csvstuff
    (
		csvFile,
        pickerBox,
        Box(..),
        Stat(..),
        dbBox,
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
	
-- Statisitcs
msum = mfoldl (+) 0.0					
mmax = mfoldl max 0.0
mmin = mfoldl min 10.0
msumsq = mfoldl (\a n -> a+n*n) 0.0
mcount = mfoldl (\a n -> a+1.0) 0.0
	
mmean::ColValue->Double	
mmean xs = (msum xs)/(mcount xs)
	
mvar::ColValue->Double	
mvar xs = (msumsq xs)/(mcount xs) - (mmean xs)**2.0
		
mstd::ColValue->Double	
mstd xs = (mvar xs)**0.5
		
data Stat = Stat {smax::Double,
			 smin::Double,
			 smean::Double,
			 ssd::Double,
			 scount::Int}

data Box = Box {bopen::Double,
			 bhi::Double,
			 blo::Double,
			 bclose::Double,
			 bcount::Int}

-- Works out the Box stats from am LValue
stat::ColValue->Stat
stat mds =  Stat {smax=mmax mds, smin=mmin mds, smean=mmean mds, ssd=mstd mds, scount=floor $ mcount mds}
		   
-- Works out the Box stats from am LValue
box::ColValue->Box
box mds =  Box {bopen=smean s + ssd s, bhi=smax s, blo=smin s, bclose=smean s - ssd s, bcount=scount s}
				where 
                    s = stat mds		   

-- Works out the Box for a column in a Tdb 
-- picking the column by name
colBox::Tdb->String->Box		 
colBox tdb name = case dselect (CellName name) tdb of
					Nothing -> Box {bopen = 0.0, bhi=0.0, blo=0.0, bclose=0.0, bcount=0}
					Just lv -> box lv

-- Works out the Box for a column in a Tdb 
-- picking the column by name
colStat::Tdb->String->Stat		 
colStat tdb name = case dselect (CellName name) tdb of
					Nothing -> Stat {smax = 0.0, smin=0.0, smean=0.0, ssd=0.0, scount=0}
					Just lv -> stat lv

-- Works out the Box stats for a all numerical columns in a Tdb 
dbBox::Tdb->Box		 
dbBox tdb = Box {bopen = lmean+lsd, bhi=maximum maxs, blo=minimum mins, bclose=lmean-lsd, bcount=sum counts}
              where
                ncols = filter isdouble tdb
                names = map fst ncols
                cs = map (colStat ncols) names
                maxs = map smax cs
                mins = map smin cs
                means = map smean cs
                vars = map (\x->x*x) (map ssd cs)
                counts = map scount cs
                lmean = sum (zipWith (*) means (map fromIntegral counts))/(fromIntegral $ sum counts)
                lsd = (sum (zipWith (*) vars (map fromIntegral counts))/(fromIntegral $ sum counts))**0.5

pickerBox::Tdb->String->Box
pickerBox tdb p = dbBox ftdb
              where
                vf = map (\c->Char c) p
                f = (CellName "Book Picker", vf)
                q = [f]
                db = fromTdb tdb
                fdb = exec q db
                ftdb = fromDb fdb


  