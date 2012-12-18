{-# LANGUAGE OverloadedStrings, Arrows #-}

import Control.Arrow ((>>>))
import Prelude hiding (id)
import Control.Category (id)
import Control.Monad (forM, forM_, liftM, liftM2)
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

import Network.FTP.Client

main :: IO ()
main = do
    -- We need the pickers so that we can make some compilers
    ss <- readFile "./csv/scoreTable.csv"
    let db = csv2db ss
    let tdb = fromDb db
    let ncols = (filter isdouble) tdb
    let pickers = (map fst) ncols

    hakyll $ do

    -- This creates an include file for the scoreTable page
    scoreTable <- match "csv/scoreTable.csv" $ do
		compile $ scoreTableCompiler
			>>> addDefaultFields

    -- This creates include files for the sorted hitParade pages
    incs <- forM pickers includeGroup
    
    -- This creates the xml file for the graph in the scoreChart page
    group("scoreChart") $ do
        match "csv/scoreTable.csv" $ do
            route $ gsubRoute "csv/scoreTable.csv" (const "xml/scoreChart.xml")
            compile $ scoreChartCompiler
                >>> addDefaultFields
                >>> applyTemplateCompiler "templates/chart.xml"
	
	
    -- This creates the xml file for the graph in the chooserChart page
    group("chooserChart") $ do
        match "csv/scoreTable.csv" $ do
            route $ customRoute (\_->"xml/chooserChart.xml")
            compile $ allChooserChartCompiler
                >>> addDefaultFields
                >>> applyTemplateCompiler "templates/chart.xml"
	
    -- This creates the xml file for the graph in the meannessChart page
    group("meannessChart") $ do
        match "csv/scoreTable.csv" $ do
            route $ customRoute (\_->"xml/meannessChart.xml")
            compile $ meannessChartCompiler
                >>> addDefaultFields
                >>> applyTemplateCompiler "templates/chart.xml"
				
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

    -- Copy all the files required for the charts - including sub directories
    match "pages/sitemap.xml" $ do
        route   idRoute
        compile copyFileCompiler

    -- Copy all the files required for the charts - including sub directories
    match "pages/robots.txt" $ do
        route   idRoute
        compile copyFileCompiler

    -- Copy all the pages
	-- Note readPageCompiler so Pandoc not called
	-- Also the csvs are all included (but there is only one becasue there is one file
	-- in the csv directory above
    match (predicate (\i -> (matches "pages/*" i) && (not (matches "pages/*Chart.html" i)) && (not (matches "pages/*.md" i)))) $ do
        route $ setExtension ".html"
        compile $ readPageCompiler
			>>> requireAll scoreTable (foldr includeFile) 
			>>> requireList incs -- This includes all the hitParade include files
            >>> addDefaultFields 
            >>> arr applySelf 
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

        -- Compile the chart pages for each reader
    match "pages/*Chart.html" $ do
        route   $ setExtension ".html"
        compile $ readPageCompiler
            >>> addDefaultFields 
            >>> arr applySelf 
            >>> applyTemplateCompiler "templates/chart.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Compile the blog pages
    match "pages/*.md" $ do
        route   $ setExtension "html"
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr applySelf
            >>> pageRenderPandoc
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Compile the xml files for the chart pages
    forM_ pickers chooserGroup
    
    where
        -- There must be a better way of doing this
        ss = readFile "./csv/scoreTable.csv"
        db = liftM csv2db ss
        tdb = liftM fromDb db
        ncols = liftM (filter isdouble) tdb
        ioPickers = liftM (map fst) ncols
        

requireList::[Pattern (Page String)]->Compiler (Page a) (Page a)
requireList ps = foldr (>>>) (head cs) (tail cs)
                where
                    cs = map ((flip requireAll) (foldr namedIncludeFile)) ps
        
chooserGroup::String->RulesM (Pattern (Page String))
chooserGroup n =  	group(n ++ "Chart") $ do
                        match "csv/scoreTable.csv" $ do
                            route $ customRoute (\_->"xml/" ++ n ++ "Chart.xml")
                            compile $ chooserChartCompiler n
                                >>> addDefaultFields
                                >>> applyTemplateCompiler "templates/chart.xml"

includeGroup::String->RulesM (Pattern (Page String))
includeGroup n = group(n) $ do
                    match "csv/scoreTable.csv" $ do
                        compile $ nameCompiler n
                            >>> addDefaultFields >>> arr (setField "name" n)

namedIncludeFile::Page String->Page a->Page a
namedIncludeFile csv_file page = setField (getField "name" csv_file) (pageBody csv_file) page

-- Include a file in a page. A file called csv/scoreTable.csv
-- will be available in the $ScoreTable$ variable. 
includeFile::Page String->Page a ->Page a
includeFile csv_file page = 
			let key=dropExtension $ takeFileName $ getField "path" csv_file
			in setField key (pageBody csv_file) page
		
		
bespokeCompiler::(String->Page String)->Compiler Resource (Page String)
bespokeCompiler = (getResourceString >>^)

scoreTableCompiler = bespokeCompiler (csv2ml show)
-- Score tale sorted according to score by reader n
nameCompiler n = bespokeCompiler (csv2ml (show.(sortdb (CellName n))))
scoreChartCompiler = bespokeCompiler (csv2ml (writeChart . fromDb))	
meannessChartCompiler = bespokeCompiler (csv2ml (writeMeannessChart . fromDb))	
-- Score chart for only books for reader called n
chooserChartCompiler n = bespokeCompiler (csv2ml ((writeChooserChart n) . fromDb))	
allChooserChartCompiler = bespokeCompiler (csv2ml ((writeAllChooserChart) . fromDb))	

-------------------------------------------------------------------------		
-- My ripoff of the readPage for csv pages
-- It parses the page and then translates to a html table
-------------------------------------------------------------------------
csv2PageDb::String->Page Db
csv2PageDb input = case parse csvFile "page" input of
						Left err -> error (show err)
						Right (b) -> Page (M.fromList []) (fromCsv b) 

csv2db::String->Db
csv2db input = case parse csvFile "page" input of
						Left err -> error (show err)
						Right (b) -> fromCsv b

csv2tdb::String->Page Tdb
csv2tdb s = fmap fromDb (csv2PageDb s)
											
-------------------------------------------------------------------------		
-- Parses the page and then translates to a the xml for a score chart
-------------------------------------------------------------------------
csv2ml::(Db->String)->String->Page String
csv2ml f input = case parse csvFile "page" input of
						Left err -> error (show err)
						Right (b) -> Page (M.fromList []) (f $ fromCsv b) 												
												
			
	
