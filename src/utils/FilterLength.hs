{-#LANGUAGE OverloadedStrings#-}

{-
  Min Zhang
  3/18/2015

  filter certain length of the reads

-}

module Main
where

import IO
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import DataTypes
import System.Environment
import Control.Applicative
import MyText

filterFastq n fq = filter ((\x-> x >= n) . T.length . fqseq) fq

argToVar [x,y,z] = (read x :: Int, y, z)

intro = TextIO.putStrLn "filterLength [length >= n] [inputPath] [outputPath]"

main = do
  intro
  (n, inpath, outpath) <- argToVar . take 3 <$> getArgs
  filterFastq n <$> importFastq inpath >>= outputFastq outpath  
