{-#LANGUAGE OverloadedStrings#-}

{-
Project name: Chromosome
Min Zhang
Date: Oct 13, 2015
Version: v.0.1.0
README: Annotate genes on sex chromosome
-}

import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import qualified Data.Char as C
import Control.Applicative
import qualified Data.List as L
import Control.Monad (fmap)
import Data.Ord (comparing)
import Data.Function (on)
import qualified Safe as S
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.Foldable as F (all)
import Data.Traversable (sequenceA)
import qualified Data.Dates as Dates
import qualified Data.ByteString.Lazy.Char8 as Bl
import qualified System.IO as IO
import System.Environment
import System.Directory
import MyText
import MyTable
import Util

main = do
  [inputpath, outputpath] <- take 2 <$> getArgs
  input <- smartTable inputpath
  chromMap <- makeChromMap
  let result = map (\x->appendChrom x chromMap) input
  TextIO.writeFile outputpath (T.unlines $ map untab result)

makeChromMap = do 
  humanGeneDescription <- smartTable "/Applications/commandline/Homer/data/accession/human.description"
  let chromosome = map getChrom $ cols 8 humanGeneDescription
  let geneNames = cols 5 humanGeneDescription
  return $ M.fromList (zip geneNames chromosome)
    where getChrom x = if T.head x == 'X' || T.head x == 'Y'
                       then T.take 1 x
                       else ""

appendChrom input chromMap = 
  let chr = M.findWithDefault "" (head input) chromMap
  in input ++ [chr]
