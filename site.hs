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
import Control.Monad (forM_, liftM, liftM2)
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
import Charts as C

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

	neil <- group("neil") $ do
		match "csv/scoreTable.csv" $ do
		compile $ nameCompiler "NEIL"
			>>> addDefaultFields

	jethro <- group("jethro") $ do
		match "csv/scoreTable.csv" $ do
		compile $ nameCompiler "JETHRO"
			>>> addDefaultFields

	iain <- group("iain") $ do
		match "csv/scoreTable.csv" $ do
		compile $ nameCompiler "IAIN"
			>>> addDefaultFields
	
-- This creates the xml file for the graph in the scoreChart page
	group("scoreChart") $ do
		match "csv/scoreTable.csv" $ do
			route $ gsubRoute "csv/scoreTable.csv" (const "xml/scoreChart.xml")
			compile $ scoreChartCompiler
				>>> addDefaultFields
				>>> applyTemplateCompiler "templates/chart.html"
	
	
-- This creates the xml file for the graph in the choserChart page
	group("chooserChart") $ do
		match "csv/scoreTable.csv" $ do
			route $ customRoute (\_->"xml/chooserChart.xml")
			compile $ chooserChartCompiler "STRETCH"
				>>> addDefaultFields
				>>> applyTemplateCompiler "templates/chart.html"
	
-- This creates the xml file for the graph in the meannessChart page
	group("meannessChart") $ do
		match "csv/scoreTable.csv" $ do
			route $ customRoute (\_->"xml/meannessChart.xml")
			compile $ meannessChartCompiler
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
			>>> requireAll neil (foldr (namedIncludeFile "neil") ) 
			>>> requireAll jethro (foldr (namedIncludeFile "jethro") ) 
			>>> requireAll iain (foldr (namedIncludeFile "iain") ) 
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
nameCompiler n = bespokeCompiler (csv2ml (show.(sortdb (CellName n))))
scoreChartCompiler = bespokeCompiler (csv2ml (write_chart . fromDb))	
meannessChartCompiler = bespokeCompiler (csv2ml (write_meanness_chart . fromDb))	
chooserChartCompiler n = bespokeCompiler (csv2ml ((write_chooser_chart n) . fromDb))	

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
												
			
{-			

:l site CsvStuff CsvParser CsvDatabase
let ss = readFile "./csv/hitParade.csv"
let dd = liftM simple_csv2db ss
let sdd = liftM (sortdb (FieldName "STRINGS")) dd
let ndd = liftM (sortdb (FieldName "STRETCH")) dd
let name = "STRETCH"
let vf = map (\c->Char c) name
let f = (FeildName "Book Picker", vf)
let q = [f]
let fdb = exec q dd
fdb


	








	
-}

	
