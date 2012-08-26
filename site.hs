{-# LANGUAGE OverloadedStrings, Arrows #-}

 {-
module Site (
	main
	,csv2ml
	,includeFile
	,scoreTableCompiler
	,scoreChartCompiler
	,csv2db
	,simple_csv2db
	,write_chart
	,write_chart_xs
	,write_chart_y,
	getField
) where
 -}

import Control.Arrow ((>>>))
import Prelude hiding (id)
import Control.Category (id)
import Control.Monad (forM_, liftM)
import Control.Arrow (arr, (>>^), (&&&), (>>>), (***), second)
import Data.List hiding (group)
import Data.Monoid (mempty, mconcat)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Numeric (readSigned, readFloat)
import System.FilePath (dropExtension, takeFileName) 

import Hakyll
import CsvParser as CP
import Csvstuff as CS
import CsvDatabase as CD

import System.IO

main :: IO ()
main = hakyll $ do

-- This creates an include file for the scoreTable page
	scoreTable <- match "csv/scoreTable.csv" $ do
		compile $ scoreTableCompiler
			>>> addDefaultFields

	strings <- group("strings") $ do
		match "csv/scoreTable.csv" $ do
		compile $ nameCompiler "STRINGS"
			>>> addDefaultFields

	stretch <- group("stretch") $ do
		match "csv/scoreTable.csv" $ do
		compile $ nameCompiler "STRETCH"
			>>> addDefaultFields

-- How do I create an include file for the hitParade page?
			
-- This creates the xml file for the graph in the scoreChart page
	group("scoreChart") $ do
		match "csv/scoreTable.csv" $ do
			route $ gsubRoute "csv/scoreTable.csv" (const "xml/scoreChart.xml")
			compile $ scoreChartCompiler
				>>> addDefaultFields
				>>> applyTemplateCompiler "templates/chart.html"
	
-- This creates the xml file for the graph in the meannessChart page
	group("meannessChart") $ do
		match "csv/scoreTable.csv" $ do
			route $ customRoute (\_->"xml/meannessChart.xml")
			compile $ meannessChartCompiler
				>>> addDefaultFields
				>>> applyTemplateCompiler "templates/chart.html"
	
-- This creates the xml file for the graph in the pickerChart page
	group("pickerChart") $ do
		match "csv/scoreTable.csv" $ do
			route $ customRoute (\_->"xml/pickerChart.xml")
			compile $ pickerChartCompiler
				>>> addDefaultFields
				>>> applyTemplateCompiler "templates/chart.html"
	
				
	-- Copy the sytle guides
	match "css/*" $ do
		route   idRoute
		compile compressCssCompiler

    -- Copy all the files required for the charts - including sub directories
	match "charts/**" $ do
		route   idRoute
		compile copyFileCompiler

    -- Compile the templates for use later
	match "templates/*" $ compile templateCompiler

    -- Copy all the pages
	-- Note readPageCompiler so Pandoc not called
	-- Also the csvs are all included (but there is only one becasue there is one file
	-- in the csv directory above
	match "pages/*" $ do
		route   $ setExtension ".html"
		compile $ readPageCompiler
			>>> requireAll scoreTable (foldr includeFile) 
			>>> requireAll strings (foldr (namedIncludeFile "strings") )
			>>> requireAll stretch (foldr (namedIncludeFile "stretch") ) 
			>>> addDefaultFields 
			>>> arr applySelf 
			>>> applyTemplateCompiler "templates/default.html"
			>>> relativizeUrlsCompiler

		

-- Include a file in a page. A fil called csv/scoreTable.csv
-- will be available in the $ScoreTable$ variable. 
includeFile::Page String->Page a ->Page a
includeFile csv_file page = 
			let key=dropExtension $ takeFileName $ getField "path" csv_file
			in setField key (pageBody csv_file) page
		
namedIncludeFile::String->Page String->Page a->Page a
namedIncludeFile name csv_file page = setField name (pageBody csv_file) page

		
bespokeCompiler::(String->Page String)->Compiler Resource (Page String)
bespokeCompiler = (getResourceString >>^)

scoreTableCompiler = bespokeCompiler (csv2ml show)
nameCompiler n = bespokeCompiler (csv2ml (show.(sortdb (FieldName n))))
scoreChartCompiler = bespokeCompiler (csv2ml (write_chart . fromDb))	
meannessChartCompiler = bespokeCompiler (csv2ml (write_meanness_chart . fromDb))	
pickerChartCompiler = bespokeCompiler (csv2ml (write_picker_chart . fromDb))	

-------------------------------------------------------------------------		
-- My ripoff of the readPage for csv pages
-- It parses the page and then translates to a html table
-------------------------------------------------------------------------
csv2db::String->Page Db
csv2db input = case parse csvFile "page" input of
						Left err -> error (show err)
						Right (b) -> Page (M.fromList []) (fromCsv b) 

simple_csv2db::String->Db
simple_csv2db input = case parse csvFile "page" input of
						Left err -> error (show err)
						Right (b) -> fromCsv b

csv2tdb::String->Page Tdb
csv2tdb s = fmap fromDb (csv2db s)
											
-------------------------------------------------------------------------		
-- Parses the page and then translates to a the xml for a score chart
-------------------------------------------------------------------------
csv2ml::(Db->String)->String->Page String
csv2ml f input = case parse csvFile "page" input of
						Left err -> error (show err)
						Right (b) -> Page (M.fromList []) (f $ fromCsv b) 												
												
------------------------------------------------------------
-- Writes a matrix of strings an xml string for a chart
------------------------------------------------------------
write_chart::Tdb-> String
write_chart tdb = 
	"<chart_data>" ++ (write_chart_xs $ head tdb) ++ (concatMap write_chart_y (filter isdouble (tail tdb))) ++ "</chart_data>\n"

write_meanness_chart::Tdb-> String
write_meanness_chart tdb = do
	let ncols = filter isdouble tdb
	let nys =  map (\c-> snd c) ncols
	let maxs = map  (\c-> mmax c) nys
	let mins = map  (\c-> mmin c) nys
	let means = map (\c->mmean c) nys
	let stds = map (\c-> mstd c) nys
	let opens = zipWith (+) means stds
	let closes = zipWith (-) means stds
	"<chart_data>" ++ (write_chart_legend ncols) ++ (write_chart_stat "max" maxs) ++ (write_chart_stat "min" mins) ++ (write_chart_stat "open" opens) ++ (write_chart_stat "close" closes) ++ "</chart_data><chart_type>candlestick</chart_type><series_color><color>333333</color></series_color>\n"

write_picker_chart::Tdb-> String
write_picker_chart tdb = do
	let ncols = filter isdouble tdb
	let nys =  map (\c-> snd c) ncols
	let maxs = map  (\c-> mmax c) nys
	let mins = map  (\c-> mmin c) nys
	let means = map (\c->mmean c) nys
	let stds = map (\c-> mstd c) nys
	let opens = zipWith (+) means stds
	let closes = zipWith (-) means stds
	"<chart_data>" ++ (write_chart_legend ncols) ++ (write_chart_stat "max" maxs) ++ (write_chart_stat "min" mins) ++ (write_chart_stat "open" opens) ++ (write_chart_stat "close" closes) ++ "</chart_data><chart_type>candlestick</chart_type><series_color><color>333333</color></series_color>\n"

-- The x has a no name and then numbers
write_chart_legend::Tdb->String
write_chart_legend ys = "<row><null/>\n" ++ concatMap (\y-> "<string>" ++ fst y ++ "</string>\n") ys ++ "</row>\n"

-- The x has a no name and then numbers
write_chart_xs::Col->String
write_chart_xs xs = "<row><null/>\n" ++ show (snd xs) ++ "</row>\n"

-- The y has a name (string) and then numbers
write_chart_y::Col->String
write_chart_y ys = "<row><string>" ++ fst ys ++ "</string>\n" ++ show (snd ys) ++ "</row>\n"

-- writes a y for max, min, open=mean-std, close=mean+std
write_chart_stat::String->[Double]->String
write_chart_stat name ys = do
	"<row><string>" ++ name ++ "</string>\n" ++ foldl (\a y -> a ++ "<number>" ++ show y ++ "</number>\n") "" (ys) ++ "</row>\n"


			
{-			

:l site CsvStuff CsvParser CsvDatabase
let ss = readFile "./csv/hitParade.csv"
let dd = liftM simple_csv2db ss
let sdd = liftM (sortdb (FieldName "STRINGS")) dd
let ndd = liftM (sortdb (FieldName "STRETCH")) dd

	








	
-}

	
