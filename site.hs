{-# LANGUAGE OverloadedStrings, Arrows #-}

import System.FilePath (dropExtension, takeBaseName, takeFileName, joinPath, splitPath) 
import Control.Arrow ((>>>))
import Prelude hiding (id)
import Control.Category (id)
import Control.Monad (forM, forM_, liftM, liftM2)
import Control.Arrow (arr, (>>^), (&&&), (>>>), (***), second)
import Data.List hiding (group)
import Data.Monoid (mempty, mconcat, mappend)
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
import Debug.Trace
import Text.Pandoc as Pandoc
import Sync


hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration
  { deployCommand = "./Sync"
  }

main :: IO ()
main = do
    -- We need the pickers so that we can loop compilers over them
    ss <- readFile "./csv/scoreTable.csv"
    let db = csv2db ss
    let tdb = fromDb db
    let ncols = (filter isdouble) tdb
    let pickers = (map fst) ncols

    hakyllWith hakyllConfig $ do

    -- Compile the templates for use later
    match "templates/*" $ compile templateCompiler

	-- All the boring copying
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "charts/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "pages/sitemap.xml" $ do
        route   idRoute
        compile copyFileCompiler

    match "pages/robots.txt" $ do
        route   idRoute
        compile copyFileCompiler

    -- This creates a scoreTable page
    match "csv/scoreTable.csv" $ do
        route $ constRoute "pages/scoreTable.html"
        compile $ do
            scoreTableCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls
    
    -- Compile the hitParade table output to nameTable.html files
    forM_ pickers hitParadeVersion

    -- Create the xml file for the graph in the scoreChart page
    match "csv/scoreTable.csv" $ version "scoreChart" $ do
        route $ gsubRoute "csv/scoreTable.csv" (const "xml/scoreChart.xml")
        compile $ do
            scoreChartCompiler
                >>= loadAndApplyTemplate "templates/chart.xml" defaultContext
	
    -- Create the xml file for the graph in the chooserChart page
    match "csv/scoreTable.csv" $ version "chooserChart" $ do
        route $ constRoute "xml/chooserChart.xml"
        compile $ do
            allChooserChartCompiler
                >>= loadAndApplyTemplate "templates/chart.xml" defaultContext
	
    -- Create the xml file for the graph in the meannessChart page
    match "csv/scoreTable.csv" $ version "meannessChart" $ do
        route $ constRoute "xml/meannessChart.xml"
        compile $ do 
            meannessChartCompiler
                >>= loadAndApplyTemplate "templates/chart.xml" defaultContext
				

    -- Create the xml files for the chart pages
    forM_ pickers chooserVersion
    
    -- Compile the chart pages for each reader
    -- The body of the *Chart.html pages is the name of the xml file that just gets put in the 
    -- *Chart.html page using the chart.html template, so that the js calls the right xml file.
    -- We should just create this output so that the html file is not needed!
    match "pages/*Chart.html" $ do
        route   $ setExtension ".html"
        compile $ do
            getResourceBody
                >>= loadAndApplyTemplate "templates/chart.html" defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Compile the blog pages - this is just a test of pandoc!
    match "pages/*.md" $ do
        route   $ setExtension "html"
        compile $ do
            p<-pandocCompiler
            debugCompiler $ show p
            pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
                   
-- Conpiles the scoreTable.csv into nameTable.html
hitParadeVersion::String->Rules ()
hitParadeVersion n =  match "csv/scoreTable.csv" $ version (n ++ "Table") $ do
                    route $ constRoute $ "pages/" ++ n ++ "Table.html"
                    compile $ do
                        nameCompiler n
                            >>= loadAndApplyTemplate "templates/default.html" defaultContext
                            >>= relativizeUrls

-- Conpiles the scoreTable.csv into nameChart.xml files for the charts package
chooserVersion::String->Rules ()
chooserVersion n =  match "csv/scoreTable.csv" $ version (n ++ "Chart") $ do
                    route $ constRoute $ "xml/" ++ n ++ "Chart.xml"
                    compile $ do
                        chooserChartCompiler n
                            >>= loadAndApplyTemplate "templates/chart.xml" defaultContext

-- Takes a string for the chooser's name and calls bespokeCompiler with a function
-- that will tuen the database fileName into html sorted for that chooser
chooserChartCompiler::String->Compiler (Item String)
chooserChartCompiler n = bespokeCompiler (csv2ml ((writeChooserChart n) . fromDb))	

-- Needs to return the html 
nameCompiler::String->Compiler (Item String)
nameCompiler n = bespokeCompiler (csv2ml (show.(sortdb (CellName n))))

scoreTableCompiler::Compiler (Item String)
scoreTableCompiler = bespokeCompiler (csv2ml show)

scoreChartCompiler::Compiler (Item String)
scoreChartCompiler = bespokeCompiler (csv2ml (writeChart . fromDb))	

meannessChartCompiler::Compiler (Item String)
meannessChartCompiler = bespokeCompiler (csv2ml (writeMeannessChart . fromDb))	

allChooserChartCompiler::Compiler (Item String)
allChooserChartCompiler = bespokeCompiler (csv2ml ((writeAllChooserChart) . fromDb))	


-- The function here goes from the database as a file name to the html
-- getResourceBody must return the database filename, I think
bespokeCompiler::(String->String)->Compiler (Item String)
bespokeCompiler f = do
                        it <- getResourceBody
                        makeItem $ f $ itemBody it


csv2PageDb::String->Db
csv2PageDb input = case parse csvFile "page" input of
						Left err -> error (show err)
						Right (b) -> fromCsv b

csv2db::String->Db
csv2db input = case parse csvFile "page" input of
						Left err -> error (show err)
						Right (b) -> fromCsv b

csv2tdb::String->Tdb
csv2tdb s = fromDb (csv2PageDb s)
											
csv2ml::(Db->String)->String->String
csv2ml f input = case parse csvFile "page" input of
						Left err -> error (show err)
						Right (b) -> (f $ fromCsv b) 												
												
			
	
