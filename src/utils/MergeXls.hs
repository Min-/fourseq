{-#LANGUAGE OverloadedStrings#-}

{-
Project name: Merge XLS files
Min Zhang
Date: Oct 13, 2015
Version: v.0.1.0
README: Merge columns from excel files. 
        This program aims for more general file merging situations.
        However, for the time being, only merge files with exact same first column (row names), have same length (row number), and only take second column. Such as htcount output, and new Ion Proton platform RNA-seq read count output.
-}

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TextIO
import qualified Data.Char as C
import Control.Applicative
import qualified Data.List as L
import Control.Monad (fmap)
import Data.Ord (comparing)
import Data.Function (on)
import qualified Safe as S
import qualified Data.Maybe as Maybe
import qualified Data.Foldable as F (all)
import Data.Traversable (sequenceA)
import qualified Data.ByteString.Lazy.Char8 as Bl
import qualified System.IO as IO
import System.Environment
import System.Directory
import MyText
import MyTable
import Util

main :: IO()
main = do
  preludeInformation
  paths <- getArgs
  let inputPaths = init paths

  -- print input path names into the first column of the file, to avoid confusion of samples
  let inputPathNames = T.concat ["samples", "\t", untab (map T.pack inputPaths), "\n"] 
  let outputPath = S.lastDef "./outputMergeXls.txt.default" paths

  -- remove first column header; transpose columns to lists
  -- TODO: checkHeader is an automation to check if first line contains numbers or letters; if former is the case, delete first line; ducktape here now, need to make this function more reliable.
  inputFiles <- map (L.transpose . checkHeader) <$> mapM smartTable inputPaths

  -- check row names of each sample are the same
  checkRowNames inputFiles
  
  -- by default, use the row name of the first sample, if it is false in checkRowNames, the output will be mismatched, although it will still print the output
  let body = mergeColumns inputFiles
  let result = T.concat [inputPathNames, body]
  TextIO.writeFile outputPath result
  
preludeInformation :: IO ()
preludeInformation = do
  putStrLn "mergeXls: merge columns from excel files. Note: The first row will be automatically removed."
  putStrLn "Usage: mergeXls input1 input2 input3 outputpath\n"

-- TODO: check if first line is header or numbers
checkHeader :: [a] -> [a]
checkHeader [] = []
checkHeader (x:xs) = if isHeader x
                     then xs
                     else (x:xs)
  where isHeader x = True

-- samples are long form (eg: [[rowname1, rowname2, rowname3, ...], [readnumber1, rn2, rn3, ...]])
checkRowNames :: Eq a => [[a]] -> IO ()
checkRowNames samples = do
  let rownames = map (S.headNote "checkRowName head, take first colunm") samples  
  if and [a==b|a<- rownames, b<-rownames] 
  then print "All samples have same rownames. Merged output will be printed."
  else print "Samples with different row names. The merged output is printed, but likely to be mismatched. Check inputs."

mergeColumns :: [[[T.Text]]] -> T.Text
mergeColumns inputFiles =
  let rownames = S.headNote "mergeColumns head call" (S.headNote "mergeColumns head call of first sample" inputFiles)
      counts = map (S.lastNote "mergeColumns last call") inputFiles
  in  T.unlines $ map untab $ L.transpose $ rownames : counts

  
