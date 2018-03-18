{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (id)
import Control.Category (id)
import Control.Monad (forM, forM_, liftM, liftM2)
import Control.Arrow (arr, (>>^), (&&&), (>>>), (***), second)
import Data.List hiding (group)
import Data.Maybe
import Data.Monoid (mempty, mconcat, mappend)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Numeric (readSigned, readFloat)
import System.FilePath (dropExtension, takeFileName)

import Hakyll
import CsvParser as CP
import CsvDatabase as CD
import Charts as C
import System.IO
import Network.FTP.Client
import Text.Pandoc as Pandoc hiding (trace)
import Database.HDBC.Sqlite3 (connectSqlite3, Connection(..))
import Database.HDBC
import Data.String.Utils
import Debug.Trace

hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration
  { deployCommand = "./Sync bookclub"
  }

main :: IO ()
main = do
  -- We need the pickers so that we can loop compilers over them
  ss <- readFile "./csv/scoreTable.csv"
  let db = csv2db ss
  let tdb = fromDb db
  let ncols = filter isdouble tdb
  let pickers = map fst ncols
------------------------------------------------------------
--
-- The update of scoreTable.csv doesn't seem to work
--
--
--
-----------------------------------------------------------


  hakyllWith hakyllConfig $ do
    -- Compile the templates for use later
    match "templates/*" $ compile templateCompiler

    -- All the boring copying
    match "csv/*" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    match "charts/**" $ do
      route   idRoute
      compile copyFileCompiler

    match "scripts/**" $ do
      route   idRoute
      compile copyFileCompiler

    match "pages/sitemap.xml" $ do
      route   idRoute
      compile copyFileCompiler

    match "pages/robots.txt" $ do
      route   idRoute
      compile copyFileCompiler

    match "pages/funcs.php" $ do
      route   idRoute
      compile copyFileCompiler

    -- Create the xml file for the graph in the scoreChart page
    -- compile :: Compiler -> Rules ()
    -- route :: Routes -> Rules ()

    -- This creates a scoreTable page
    match "csv/scoreTable.csv" $ version "htmlVersion" $ do
      route $ constRoute "pages/scoreTable.html"
      compile $
        scoreTableCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

    match "csv/scoreTable.csv" $ version "scoreChart" $ do
      let gsr = trace "*** Calculating route for scoreChart..." $ gsubRoute "csv/scoreTable.csv" (const "xml/scoreChart.xml")
      let comp = trace "*** Calculating compiler for scoreChart..." $ scoreChartCompiler >>= loadAndApplyTemplate "templates/chart.xml" defaultContext
      trace "*** Calling route for scoreChart..." $ route gsr
      trace "*** Calling compile for scoreChart..." $ compile comp

    -- Create the xml file for the graph in the chooserChart page
    match "csv/scoreTable.csv" $ version "chooserChart" $ do
      route $ constRoute "xml/chooserChart.xml"
      compile $
        allChooserChartCompiler
          >>= loadAndApplyTemplate "templates/chart.xml" defaultContext

    -- Create the xml file for the graph in the meannessChart page
    match "csv/scoreTable.csv" $ version "meannessChart" $ do
      route $ constRoute "xml/meannessChart.xml"
      compile $
        meannessChartCompiler
          >>= loadAndApplyTemplate "templates/chart.xml" defaultContext

    -- Compile the hitParade table output to nameTable.html files
    forM_ pickers (hitParadeVersion defaultContext )
    -- Create the xml files for the chart pages
    forM_ pickers (chooserVersion defaultContext )

    -- Compile the chart pages for each reader
    -- The body of the *Chart.html pages is the name of the xml file that just gets put in the
    -- *Chart.html page using the chart.html template, so that the js calls the right xml file.
    -- We should just create this output so that the html file is not needed!
    match "pages/*Chart.html" $ do
      route   $ setExtension ".html"
      compile $
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
-- compile gets called but not nameCompiler !?!
hitParadeVersion::Context String->String->Rules ()
hitParadeVersion cxt n =  trace ("*** Reached hitParadeVersion with: " ++ n) $ match "csv/scoreTable.csv" $ version (n ++ "Table") $ do
    trace "*** Calling route in hitParadeVersion" $ route $ constRoute $ "pages/" ++ n ++ "Table.html"
    trace "*** Calling compile in hitParadeVersion" $ compile $ do
        nc <- trace ("*** Calling nameCompiler in hitParadeVersion with: " ++ n) $ nameCompiler n
        debugCompiler $ show nc
        -- return nc >>= loadAndApplyTemplate "templates/default.html" cxt >>= relativizeUrls
        loadAndApplyTemplate "templates/default.html" cxt nc >>= relativizeUrls

-- Conpiles the scoreTable.csv into nameChart.xml files for the charts package
chooserVersion::Context String->String->Rules ()
chooserVersion cxt n =  trace ("*** Reached chooserVersion for " ++ n) $ match "csv/scoreTable.csv" $ version (n ++ "Chart") $ do
                    route $ constRoute $ "xml/" ++ n ++ "Chart.xml"
                    compile $
                        chooserChartCompiler n
                            >>= loadAndApplyTemplate "templates/chart.xml" cxt

-- Takes a string for the chooser's name and calls bespokeCompiler with a function
-- that will tuen the database fileName into html sorted for that chooser
chooserChartCompiler::String->Compiler (Item String)
chooserChartCompiler n = trace ("*** In chooserChartCompiler for " ++ n) $ bespokeCompiler (csv2ml (writeChooserChart n . fromDb))

-- Needs to return the html
nameCompiler::String->Compiler (Item String)
nameCompiler n = trace "*** In nameCompiler" $ bespokeCompiler (csv2ml (dshow.sortdb (CellName n)))

scoreTableCompiler::Compiler (Item String)
scoreTableCompiler = trace "*** In scoreTableCompiler" $ bespokeCompiler (csv2ml dshow)

scoreChartCompiler::Compiler (Item String)
scoreChartCompiler = trace "*** In scoreChartCompiler" $ bespokeCompiler (csv2ml (writeChart . fromDb))

meannessChartCompiler::Compiler (Item String)
meannessChartCompiler = trace "*** In meannessChartCompiler" $ bespokeCompiler (csv2ml (writeMeannessChart . fromDb))

allChooserChartCompiler::Compiler (Item String)
allChooserChartCompiler = trace "*** In allChooserChartCompiler" $ bespokeCompiler (csv2ml (writeAllChooserChart . fromDb))


-- The function here goes from the database as a file name to the html
-- getResourceBody must return the database filename, I think
bespokeCompiler::(String->String)->Compiler (Item String)
bespokeCompiler f = do
                        it <- getResourceBody
                        makeItem $ f $ itemBody it

csv2db::String->Db
csv2db = csv2ml id

csv2tdb::String->Tdb
csv2tdb s = fromDb (csv2db s)

csv2ml :: (Db->a)->String->a
csv2ml f input = either (error.show) (f.fromCsv) $ parse csvFile "page" input
