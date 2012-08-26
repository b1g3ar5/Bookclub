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
main = do
	ss <- readFile "./csv/hitParade.csv"
	cc <- parse csvFile "page" ss
	putStrLn ss
	putStrLn cc
	--dd <- fromCsv cc
	--sdd <- (sortdb (FieldName "STRINGS")) 
	--ndd <- (sortdb (FieldName "STRETCH")) dd
	--show sdd
	--show ndd

simple_csv2db::String->Db
simple_csv2db input = case parse csvFile "page" input of
						Left err -> error (show err)
						Right (b) -> fromCsv b

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
						
						