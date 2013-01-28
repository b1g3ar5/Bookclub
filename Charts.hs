{-# LANGUAGE OverloadedStrings, Arrows #-}

module Charts (
	writeChart
	,writeMeannessChart
	,writeAllChooserChart
	,writeChooserChart
	,writeChartXs
	,writeChartY
) where

import Control.Arrow ((>>>))
import Prelude hiding (id)
import Control.Category (id)
import Control.Monad (forM_, liftM, liftM2)
import Control.Arrow (arr, (>>^), (&&&), (>>>), (***), second)
import Data.List hiding (group)
import Data.Monoid (mempty, mconcat)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Numeric (readSigned, readFloat)
import System.FilePath (dropExtension, takeFileName) 

import Hakyll
import CsvParser --as CP
import Csvstuff --as CS
import CsvDatabase --as CD
import CsvStats

import System.IO


------------------------------------------------------------
-- Writes a matrix of strings an xml string for a chart
------------------------------------------------------------
writeChart::Tdb-> String
writeChart tdb = 
	"<chart_data>" ++ (writeChartXs $ head tdb) ++ (concatMap writeChartY (filter isdouble (tail tdb))) ++ "</chart_data>\n"

-- Writes the xml for the chart of stats for each persons marks
-- open, high, lo, close for each column
writeMeannessChart::Tdb-> String
writeMeannessChart tdb = do
	let ncols = filter isdouble tdb
	let nys =  map (\c-> snd c) ncols
	let maxs = map  (\c-> mmax c) nys
	let mins = map  (\c-> mmin c) nys
	let means = map (\c->mmean c) nys
	let stds = map (\c-> mstd c) nys
	let opens = zipWith (+) means stds
	let closes = zipWith (-) means stds
	title "Scores Given" ++ "<chart_data>" ++ (writeChartLegend ncols) ++ (writeChartStat "max" maxs) ++ (writeChartStat "min" mins) ++ (writeChartStat "open" opens) ++ (writeChartStat "close" closes) ++ "</chart_data><chart_type>candlestick</chart_type><series_color><color>333333</color></series_color>\n"

title::String->String
title t = "<draw><text shadow='shadow1' color='ffffee' alpha='75' rotation='0' size='50' x='40' y='15' width='600' height='50' h_align='left' v_align='top'>" ++ t ++ "</text></draw>"

-- Writes the same mark chart as the previous but only for marks of
-- the book chooser, p
writeChooserChart::String->Tdb-> String
writeChooserChart p tdb = do
	let vf = map (\c->Char c) p
	let f = (CellName "Book Picker", vf)
	let q = [f]
	let db = fromTdb tdb
	let fdb = exec q db
	let ftdb = fromDb fdb
	let ncols = filter isdouble ftdb
	let nys =  map (\c-> snd c) ncols
	let maxs = map  (\c-> mmax c) nys
	let mins = map  (\c-> mmin c) nys
	let means = map (\c->mmean c) nys
	let stds = map (\c-> mstd c) nys
	let opens = zipWith (+) means stds
	let closes = zipWith (-) means stds
	title (p ++ "'s Choices") ++ "<chart_data>"  ++ (writeChartLegend ncols)  ++ (writeChartStat "max" maxs) ++ (writeChartStat "min" mins) ++ (writeChartStat "open" opens) ++ (writeChartStat "close" closes) ++ "</chart_data><chart_type>candlestick</chart_type><series_color><color>333333</color></series_color>\n"
	
-- Writes the same mark chart as the previous but for all the marks of
-- the book choosen by each picker
writeAllChooserChart::Tdb-> String
writeAllChooserChart tdb = title "Scores of Books Chosen" ++ "<chart_data>"  ++ (writeChartLegend ncols)  ++ (writeChartStat "max" maxs) ++ (writeChartStat "min" mins) ++ (writeChartStat "open" opens) ++ (writeChartStat "close" closes) ++ "</chart_data><chart_type>candlestick</chart_type><series_color><color>333333</color></series_color>\n"
        where 
            ncols = filter isdouble tdb
            pickers = map fst ncols
            bs = map (pickerBox tdb) pickers
            opens = map bopen bs
            maxs = map bhi bs
            mins = map blo bs
            closes = map bclose bs

	
-- The x has a no name and then numbers
writeChartLegend::Tdb->String
writeChartLegend ys = "<row><null/>\n" ++ concatMap (\y-> "<string>" ++ fst y ++ "</string>\n") ys ++ "</row>\n"

-- The x has a no name and then numbers
writeChartXs::Col->String
writeChartXs xs = "<row><null/>\n" ++ show (snd xs) ++ "</row>\n"

-- The y has a name (string) and then numbers
writeChartY::Col->String
writeChartY ys = "<row><string>" ++ fst ys ++ "</string>\n" ++ show (snd ys) ++ "</row>\n"

-- writes a y for max, min, open=mean-std, close=mean+std
writeChartStat::String->[Double]->String
writeChartStat name ys = do
	"<row><string>" ++ name ++ "</string>\n" ++ foldl (\a y -> a ++ "<number>" ++ show y ++ "</number>\n") "" (ys) ++ "</row>\n"


