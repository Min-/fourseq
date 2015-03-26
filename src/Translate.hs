{-#LANGUAGE OverloadedStrings#-}

{-
  Min Zhang
  3-26-2015

  Translate DNA/RNA to protein aa sequence.
-}

module Main
where

import qualified Data.Text.IO as TextIO
import qualified Data.Text as T
import Control.Applicative
import System.Environment
import Data.Monoid

import Dna
import IO
import DataTypes

translate :: T.Text -> T.Text
translate "" = mempty
translate rna = codon (T.take 3 rna) `mappend` translate (T.drop 3 rna)

codon :: T.Text -> T.Text
codon r
  |T.take 2 r == "UU" = if lastUC r then "F" else "L"
  |T.take 2 r == "UC" = "S"
  |T.take 2 r == "UA" = if lastUC r then "Y" else "s"
  |T.take 2 r == "UG" = if lastUC r then "C" else if T.last r == 'A' then "s" else "W"
  |T.take 2 r == "CU" = "L"
  |T.take 2 r == "CC" = "P"
  |T.take 2 r == "CA" = if lastUC r then "H" else "Q"
  |T.take 2 r == "CG" = "R"
  |T.take 2 r == "AU" = if lastUC r then "I" else if T.last r == 'A' then "I" else "M"
  |T.take 2 r == "AC" = "T"
  |T.take 2 r == "AA" = if lastUC r then "N" else "K"
  |T.take 2 r == "AG" = if lastUC r then "S" else "R"
  |T.take 2 r == "GU" = "V"
  |T.take 2 r == "GC" = "A"
  |T.take 2 r == "GA" = if lastUC r then "D" else "E"
  |T.take 2 r == "GG" = "G"
  |otherwise = "x"

lastUC r = T.last r == 'U' || T.last r == 'C'

faDnaToRna fa = Fasta (faname fa) ((dnaToRna . faseq) fa)
faRnaToProtein fa = Fasta (faname fa) ((T.init . translate . faseq) fa)

intro = do
  TextIO.putStrLn "Translate RNA/DNA to protein sequence.\n"
  TextIO.putStrLn "Translate [input DNA Fasta] [output Fasta]\n"  

-- IO
readRNA inpath outpath = do
  dna <- map faDnaToRna <$> importFasta inpath 
  let protein = map faRnaToProtein dna
  outputFasta outpath protein 

main = do
  intro
  [inpath, outpath] <- take 2 <$> getArgs
  readRNA inpath outpath
   
