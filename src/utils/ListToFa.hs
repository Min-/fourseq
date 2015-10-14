{-#LANGUAGE OverloadedStrings#-}

{-
  Min Zhang
  3-29-2015
  ListToFa
  
  Turn tab separated tables to Fasta and can be loaded to UCSC blat mapping.
-}

module Main
where


import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TextIO
import Data.List (foldl')
import System.Environment
import Control.Applicative
import qualified Data.Map as M
import Data.Monoid

import IO
import DataTypes
import MyText

main = do
  [input, output] <- take 2 <$> getArgs
  fa <- map listToFasta <$> textToList input
  outputFasta output fa 

textToList :: FilePath -> IO [[T.Text]]
textToList input = do
  a <- map tab . T.lines <$> TextIO.readFile input
  return a

listToFasta :: [T.Text] -> Fasta
listToFasta [a, b] = Fasta (T.concat [">", a]) b
