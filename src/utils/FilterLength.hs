{-#LANGUAGE OverloadedStrings#-}

{-
  Min Zhang
  3/18/2015

  filter certain length of the reads

  v.0.1.0 (Oct 14, 2015): switch from Data.Text to Data.Text.Lazy 
-}

module Main
where

import IO
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TextIO
import DataTypes
import System.Environment
import Control.Applicative
import MyText

filterFastq n fq = filter ((\x-> x >= n) . fromIntegral . T.length . fqseq) fq

argToVar [x,y,z] = (read x :: Int, y, z)

intro = TextIO.putStrLn "filterLength [length >= n] [inputPath] [outputPath]"

main = do
  intro
  (n, inpath, outpath) <- argToVar . take 3 <$> getArgs
  filterFastq n <$> importFastq inpath >>= outputFastq outpath  
