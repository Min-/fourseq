{-#LANGUAGE OverloadedStrings#-}

{-
Project name: Remove rRNA from annotated read count list.
Min Zhang
Date: Oct 21, 2015
Version: v.0.1.0
README: 
#Biomart Homo sapiens genes (GRCh38.p3)
#Filters:Mt_rRNA, Mt_tRNA, rRNA, snoRNA, snRNA 

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
import qualified Data.Set as Set
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.Foldable as F (all)
import Data.Traversable (sequenceA)
import qualified System.IO as IO
import System.Environment
import System.Directory
import MyText
import MyTable
import Util

hg38 = do
  input <- drop 3 <$> smartTable "/Users/minzhang/Documents/private_git/fourseq/share/genome/hg38.rrna.txt"
  return $ Set.fromList $ map last input

hg19refseq =
  let inputpath = "/Users/minzhang/Documents/private_git/fourseq/share/genome/refseq_hg19_geneName_symbol.txt"
  in
  smartTable inputpath >>= return . Set.fromList . map last . tail


{-
main = do
  [inputPath, outputPath] <- getArgs
  hg38rRNAset <- hg38
  input <- smartTable inputPath
  let header = head input
  let body = filter (\x->not $ Set.member (head x) hg38rRNAset) (tail input)
  let output = header : body
  TextIO.writeFile outputPath (T.unlines (map untab output))
-}

-- include the genes in the hg19refseq
main = do
  [inputPath, outputPath] <- getArgs
  hg19genesym <- hg19refseq
  input <- smartTable inputPath
  let header = head input
  let body = filter (\x->Set.member (head x) hg19genesym) (tail input)
  let output = header : body
  TextIO.writeFile outputPath (T.unlines (map untab output))

